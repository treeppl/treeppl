include "map.mc"
include "math.mc"
include "seq.mc"
include "matrix.mc"
include "tensor.mc"

----------------
--- Messages ---
----------------

-- Labels are integers, as this is the most efficient option. We currently
-- assume that the mapping between actual string labels and integers is done in
-- pre- and post-processing.
type Label = Int

-- Always three elements, but not expressed in type
type Message = [Tensor[Float]]

-- Alias
let tc = tensorCreateCArrayFloat

-- Vector normalization
let normalizeVector: Tensor[Float] -> Tensor[Float] = lam v.
  let sum = tensorFold addf 0. v in
  tc (tensorShape v) (lam i. divf (tensorGetExn v i) sum)

-- Mean of a tensor (e.g., vectors and matrices)
let tensorMean : Tensor[Float] -> Float = lam t.
  divf (tensorFold addf 0. t) (int2float (tensorSize t))

-- Sequence normalization
let normalize: [Float] -> [Float] = lam seq.
  let sum = foldl addf 0. seq in
  map (lam f. divf f sum) seq

let normalizeMessage: Message -> Message =
  lam m. map normalizeVector m

-- Elementwise multiplication of state likelihoods/probabilities
let mulMessage: Message -> Message -> Message = zipWith matrixElemMul

-- Raises each element to the power of the float argument
let messageElementPower: Message -> Float -> Message =
  lam m. lam f. map (lam v.
    tc (tensorShape v) (lam i. pow (tensorGetExn v i) f)
  ) m

-------------
--- Trees ---
-------------

-- TODO: There is lots of repetition for the Tree types. Make the Tree type
-- Tree l r instead, and instantiate l and r to suitable types for the
-- different Tree types?
type Tree
con Leaf: {
  age: Float,
  label: Label
} -> Tree
con Node: {
  age: Float,
  left: Tree,
  right: Tree,
  label: Label
} -> Tree

type MsgTree
con MsgLeaf: {
  age: Float,
  label: Label,
  out_msg: Message
} -> MsgTree
con MsgNode: {
  age: Float,
  label: Label,
  left: MsgTree,
  right: MsgTree,
  left_in_msg: Message,
  right_in_msg: Message,
  out_msg: Message
} -> MsgTree

let getOutMsg: MsgTree -> Message = lam tree.
  match tree with MsgLeaf l then l.out_msg
  else match tree with MsgNode n then n.out_msg
  else never

let getMsgTreeAge: MsgTree -> Float = lam tree.
  match tree with MsgLeaf l then l.age
  else match tree with MsgNode n then n.age
  else never

type ProbsTree
con ProbsLeaf: {
  age: Float,
  label: Label,
  probs: Message
} -> ProbsTree
con ProbsNode: {
  age: Float,
  label: Label,
  probs: Message,
  left: ProbsTree,
  right: ProbsTree
} -> ProbsTree

let getProbs: ProbsTree -> Message = lam tree.
  match tree with ProbsLeaf l then l.probs
  else match tree with ProbsNode n then n.probs
  else never

let getProbsAge: ProbsTree -> Float = lam tree.
  match tree with ProbsLeaf l then l.age
  else match tree with ProbsNode n then n.age
  else never

type Event = {age: Float, host: Int, from_state: Int, to_state: Int}

type HistoryPoint = {age: Float, repertoire: [Int]}

type HistoryTree
con HistoryLeaf: {
  age: Float,
  label: Label,
  repertoire: [Int],
  history: [HistoryPoint]
} -> HistoryTree
con HistoryNode: {
  age: Float,
  label: Label,
  repertoire: [Int],
  history: [HistoryPoint],
  left: HistoryTree,
  right: HistoryTree
} -> HistoryTree

------------------------
--- Model parameters ---
------------------------

type ModelParams = {
  q: Tensor[Float],
  d_matrix: Tensor[Float],
  d_average: Float,
  beta: Float
}


mexpr

-----------------------
--- Model functions ---
-----------------------

let observation_message: Tensor[Char] -> Message =
  lam interactions.
    reverse (tensorFold (lam acc. lam c.
        cons
          (switch c
            case '0' then rvecCreate 3 [1.,0.,0.]
            case '1' then rvecCreate 3 [0.,1.,0.]
            case '2' then rvecCreate 3 [0.,0.,1.]
            case '?' then rvecCreate 3 [1.,1.,1.]
            case _ then error "Unknown character"
          end) acc
      ) [] interactions)
in

let message: Message -> Tensor[Float] -> Message =
  lam start_msg. lam p.
    -- Assumption: v is a row vector
    map (lam v. matrixMul v p) start_msg
in

recursive let postorder_msgs:
  Tree -> Tensor[Char] -> Tensor[Float] -> MsgTree =
    lam tree. lam interactions. lam q.
    match tree with Leaf t then
      MsgLeaf {
        age = 0.,
        label = t.label,
        out_msg = observation_message (tensorSubExn interactions t.label 1)
      }
    else match tree with Node t then
      let left = postorder_msgs t.left interactions q in
      let right = postorder_msgs t.right interactions q in

      let leftAge = getMsgTreeAge left in
      let rightAge = getMsgTreeAge right in

      let tLeft =
        matrixTranspose
          (matrixExponential (matrixMulFloat (subf t.age leftAge) q)) in
      let tRight =
        matrixTranspose
          (matrixExponential (matrixMulFloat (subf t.age rightAge) q)) in

      let left_in_msg  = message (getOutMsg left) tLeft in
      let right_in_msg = message (getOutMsg right) tRight in

      let out_msg = normalizeMessage (mulMessage left_in_msg right_in_msg) in

      MsgNode {
        age=t.age,
        label=t.label,
        left=left,
        right=right,
        left_in_msg=left_in_msg,
        right_in_msg=right_in_msg,
        out_msg=out_msg
      }
    else never
in

recursive let final_probs:
  MsgTree -> Message -> Tensor[Float] -> Float -> ProbsTree =
    lam tree. lam root_msg. lam q. lam tune.

    let probs: Message =
      normalizeMessage
        (messageElementPower (mulMessage (getOutMsg tree) root_msg) tune) in

    match tree with MsgLeaf t then
      ProbsLeaf { age = 0., label = t.label, probs = probs }
    else match tree with MsgNode t then
      let leftAge = getMsgTreeAge t.left in
      let rightAge = getMsgTreeAge t.right in

      let tLeft = matrixExponential (matrixMulFloat (subf t.age leftAge) q) in
      let tRight = matrixExponential (matrixMulFloat (subf t.age rightAge) q) in

      let left_root_msg =
        message (mulMessage root_msg (t.right_in_msg)) tLeft in
      let right_root_msg =
        message (mulMessage root_msg (t.left_in_msg)) tRight in

      let left = final_probs (t.left) left_root_msg q tune in
      let right = final_probs (t.right) right_root_msg q tune in

      ProbsNode { age=t.age, label=t.label, left=left, right=right, probs=probs }
    else never
in

let rate: [Int] -> Int -> Int -> ModelParams -> Float =
  lam rep. lam host_index. lam to_state. lam mp.
    let from_state: Int = get rep host_index in

    let base_rate = tensorGetExn mp.q [from_state,to_state] in

    if gti from_state to_state then
      let c =
        foldl (lam acc. lam e. if eqi e 2 then addi acc 1 else acc) 0 rep in
      if and (eqi from_state 2) (eqi c 1) then
        0.
      else
        base_rate
    else

      let current_hosts: [Int] =
        if eqi from_state 0 then
          -- current_hosts = which(rep %in% [1,2])
          indices (lam h. if leqi h 2 then geqi h 1 else false) rep
        else
          -- current_hosts = which(rep == 2)
          indices (eqi 2) rep
      in

      -- d = mean (mp.D[host_index][current_hosts])
      let d =
        tensorMean (tensorSubSeqExn tc mp.d_matrix [[host_index],current_hosts])
      in

      mulf base_rate (exp (negf (mulf mp.beta (divf d mp.d_average))))

in

let total_rate: [Int] -> ModelParams -> Float =
  lam rep. lam mp.
    let lossRates1 =
      mulf (int2float (length (indices (eqi 1) rep)))
        (tensorGetExn mp.q [1,0]) in
    let lossRates2 =
      let l = length (indices (eqi 2) rep) in
      if eqi 1 l then 0.
      else mulf (int2float l) (tensorGetExn mp.q [2,1])
    in

    let gainRates =
      foldli (lam acc. lam i. lam r.
          if or (eqi r 0) (eqi r 1) then
            addf acc (rate rep i (addi (get rep i) 1) mp)
          else acc
        ) 0. rep
    in

    addf (addf lossRates1 lossRates2) gainRates
in

recursive let simulate_by_event:
  [Int] -> [Event] -> Int -> Float -> Float -> ModelParams -> [HistoryPoint] =
    lam rep. lam events. lam event_index. lam from_age. lam end_age. lam mp.
      let l = length events in
      if geqi event_index l then
        let change_rate: Float = total_rate rep mp in
        let start_age = if null events then from_age else (last events).age in
        observe 0 (Poisson (mulf change_rate
                              (subf start_age end_age)));
        []
      else
        let the_event: Event = get events event_index in
        let rate = rate rep (the_event.host) (the_event.to_state) mp in
        let change_rate = total_rate rep mp in

        observe true (Bernoulli (divf rate change_rate));
        observe (subf from_age (the_event.age)) (Exponential change_rate);

        let new_rep = mapi (lam i. lam r.
            if eqi i (the_event.host) then the_event.to_state
            else r
          ) rep
        in

        let hp: HistoryPoint = { age = the_event.age, repertoire = new_rep } in

        cons hp
          (simulate_by_event new_rep events
             (addi event_index 1) the_event.age end_age mp)
in



let propose_exponential_max_t: Float -> Float -> Float =
  lam rate. lam max_t.
    let u_min = exp (negf (mulf rate max_t)) in
    -- This is equivalent to a propose with immediate
    -- repay
    let u = assume (Uniform u_min 1.0) in
    weight (log (subf 1.0 b));
    divf (negf(log u)) rate
in

recursive let propose_events_for_host:
  Int -> Float -> Float -> Int -> Int -> Tensor[Float] -> [Event] =
    lam host_index. lam from_age. lam end_age.
    lam from_state. lam end_state. lam q.
      let rate = negf (tensorGetExn q [from_state, from_state]) in

      let t =
        if neqi from_state end_state then
          propose_exponential_max_t rate (subf from_age end_age)
        else
          -- This should use `propose` eventually, now `assume` and `weight`
          -- manually.
          -- TODO Handle proposal debts
          assume (Exponential rate)
      in
      let new_age = subf from_age t in

      if ltf new_age end_age then
        []
      else
        let to_states =
          switch from_state
            case 0 then [1,2]
            case 1 then [0,2]
            case 2 then [0,1]
          end
        in

        let state_probs = normalize [
          tensorGetExn q [from_state, get to_states 0],
          tensorGetExn q [from_state, get to_states 1]
        ] in

        -- This should use `propose` eventually, now `assume` and `weight`
        -- manually.
        -- TODO Handle proposal debts
        let new_state = assume (Categorical (state_probs)) in
        let new_state = get to_states new_state in

        let event: Event = {
          age = new_age, host = host_index,
          from_state = from_state, to_state = new_state
        } in

        cons event
          (propose_events_for_host
             host_index new_age end_age new_state end_state q)
in


recursive let propose_events:
  Int -> HistoryPoint -> HistoryPoint -> Tensor[Float] -> [Event] =
    lam host_index. lam from. lam to. lam q.
      if (geqi host_index (length from.repertoire)) then
        []
      else
        let events = propose_events_for_host host_index from.age to.age
                       (get from.repertoire host_index)
                       (get to.repertoire host_index) q in
        concat events (propose_events (addi host_index 1) from to q)
in

let simulate_history:
  HistoryPoint -> HistoryPoint -> ModelParams -> [HistoryPoint] =
    lam from. lam to. lam mp.
      let unordered_events: [Event] = propose_events 0 from to (mp.q) in
      let cmp = lam x. lam y. if gtf x.age y.age then 1 else negi 1 in
      let events = sort cmp unordered_events in
      let rep = from.repertoire in
      simulate_by_event rep events 0 from.age to.age mp
in

recursive let simulate: ProbsTree -> HistoryPoint -> ModelParams -> HistoryTree =
 lam tree. lam start. lam mp.
   let probs: Message = getProbs tree in
   let rep: [Int] = map (lam p.
     -- This should use `propose` eventually, now `assume` and `weight` manually
     -- TODO Handle proposal debts
     assume (Categorical (vecToSeqExn p))
   ) probs in

   let stop: HistoryPoint = { age = getProbsAge tree, repertoire = rep } in
   let history = simulate_history start stop mp in

   match tree with ProbsLeaf l then
     HistoryLeaf {
      age = l.age, label = l.label, repertoire = rep, history = history
     }
   else match tree with ProbsNode n then
     let left = simulate n.left stop mp in
     let right = simulate n.right stop mp in
     HistoryNode {
       age = n.age, label = n.label, repertoire = rep,
       history = history, left = left, right = right
     }
   else never
in

let get_proposal_params:
  Tree -> Tensor[Char] -> Tensor[Float] -> Tensor[Float] -> Float -> ProbsTree =
    lam parasite_tree. lam interactions. lam q. lam stationary_probs. lam tune.

      let msgTree: MsgTree = postorder_msgs parasite_tree interactions q in

      let pis: Message = create (length (getOutMsg msgTree)) (lam. stationary_probs) in

      final_probs msgTree pis q tune
in

------------------------------
--- Input data (hardcoded) ---
------------------------------

let parasite_tree: Tree = Node{
  age = 10.0,
  left = Node {
    age = 3.360348,
    left = Node{
      age = 1.279190,
      left = Leaf{age = 0.0, label = 0},
      right = Leaf{age = 0.0, label = 1},
      label = 6
    },
    right = Node{
      age = 0.153814,
      left = Leaf{age = 0.0, label = 2},
      right = Leaf{age = 0.0, label = 3},
      label = 7
    },
    label = 8
  },
  right = Node {
    age = 1.079144,
    left = Leaf{age = 0.0, label = 4},
    right = Leaf{age = 0.0, label = 5},
    label = 9
  },
  label = 10
} in

let interactions: Tensor[Char] = tensorOfSeqExn tensorCreate [6,5] [
  -- Row labels: T1, T2, T3, T4, T5, T6
  -- Column labels: H1, H2, H3, H4, H5
  '2','2','0','0','0',
  '2','2','0','0','0',
  '0','0','2','2','0',
  '0','0','2','2','0',
  '0','0','0','0','2',
  '0','0','0','0','2'
] in

let host_distances: Tensor[Float] = matrixCreate [5,5] [
  -- Row and column labels: H1, H2, H3, H4, H5
  0.,0.8630075756,2.6699063134,2.6699063134,2.6699063134,
  0.8630075756,0.,2.6699063134,2.6699063134,2.6699063134,
  2.6699063134,2.6699063134,0.,1.2256551506,1.9598979474,
  2.6699063134,2.6699063134,1.2256551506,0.,1.9598979474,
  2.6699063134,2.6699063134,1.9598979474,1.9598979474,0.
] in

let tune: Float = 0.9 in

-------------------------
--- Model entry point ---
-------------------------
let lambda: [Float] = assume (Dirichlet [1.,1.,1.,1.]) in
let mu: Float = assume (Exponential 10.) in
let beta: Float = assume (Exponential 1.) in

let r: Tensor[Float] = matrixCreate [3,3] [
  negf (get lambda 0), (get lambda 0), 0.,
  get lambda 1, negf (addf (get lambda 1) (get lambda 2)), get lambda 2,
  0., get lambda 3, negf (get lambda 3)
] in

let q: Tensor[Float] = matrixMulFloat mu r in

-- Hardcoded, should be done in preprocessing --
let n_hosts = 5 in -- Not needed anywhere it seems
let d_matrix = host_distances in
let d_average = 4.4 in
------------------------------------------------

let mp: ModelParams =
  { q = q, d_matrix = d_matrix, d_average = d_average, beta = beta } in

-- Fredriks notes below for the calculation of stationary:probs
-- Assume 0-indexing.
-- lambda:[Float] -- Contains lambda_01, lambda_10, lambda_12, lambda_21
-- pi:[Float] -- Contains the stationary probs
-- -- You can do this in any order, I start with pi[1]. Sorry for the procedural style...
-- pi[1] = 1. / (1.0 + (lambda[1]/lambda[0]) + (lambda[2]/lambda[3]))
-- pi[0] = pi[1] * (lambda[1]/lambda[0])
-- pi[2] = 1. - pi[0] - pi[1]
let pi1 = divf 1. (addf (addf 1. (divf (get lambda 1) (get lambda 0)))
                     (divf (get lambda 2) (get lambda 3))) in
let pi0 = mulf pi1 (divf (get lambda 1) (get lambda 0)) in
let pi2 = subf (subf 1. pi0) pi1 in
let stationary_probs = rvecCreate 3 [pi0,pi1,pi2] in

let probs_tree: ProbsTree =
  get_proposal_params parasite_tree interactions q stationary_probs tune in

let rep: [Int] = map (lam p.
    -- This should use `propose` eventually, now `assume` and `weight` manually
    -- TODO Handle proposal debts
    assume (Categorical (vecToSeqExn p))
  ) (getProbs probs_tree) in

(
  if any (eqi 2) rep then
    iter (lam r. observe r (Categorical (vecToSeqExn stationary_probs))) rep
  else
    weight (negf inf)
);

match probs_tree with ProbsNode n then
  let historyPoint: HistoryPoint = { age = n.age, repertoire = rep } in
  let left = simulate n.left historyPoint mp in
  let right = simulate n.right historyPoint mp in
  let historyTree = HistoryNode {
    age = n.age,
    label = n.label,
    repertoire = rep,
    history = [],
    left=left,
    right=right
  } in

  mu
  -- TODO We currently only return mu, but the actual return value should be:
  -- { historyTree = historyTree, lambda = lambda, mu = mu, beta = beta }

else error "Root is not a node!"

