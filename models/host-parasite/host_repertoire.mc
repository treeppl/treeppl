include "map.mc"
include "math.mc"

----------------
--- Matrices ---
----------------

-- TODO Matrix stuff, we need to implement this (in the Miking standard library). We
-- should probably do it for tensors instead (and also add support for tensors
-- in CorePPL)
--
-- TODO Check how matrices are handled in OCaml, implement externals?
type Matrix a = [[a]] -- TODO Tensor[a] instead of [[a]]?

let transpose: Matrix Float -> Matrix Float =
  lam m. never -- TODO

let matrixExponential: Matrix Float -> Matrix Float =
  lam m. never -- TODO Daniel. From Fredrik below. Or use some external.
     -- This subroutine is a translation of the ALGOL procedure HQR2,
     -- Num. Math. 14, 219,231(1970) by Martin, Peters, and Wilkinson.
     -- Handbook for Automatic Computation, vol. II - Linear Algebra,
     -- pp. 357-391 (1971).

let matrixMulScalar: Float -> Matrix Float -> Matrix Float =
   lam s. lam m.
    map (lam row. map (lam e. mulf s e) row) m

let matrixMul: Matrix Float -> Matrix Float -> Matrix Float =
  lam m1. lam m2. never -- TODO

----------------
--- Messages ---
----------------

-- Labels are integers, as this is the most efficient option. We currently
-- assume that the mapping between actual string labels and integers is done in
-- pre- and post-processing.
type Label = Int

-- Always three elements, but not expressed in type
type Message = [[Float]]

-- Standard normalization. Should also return the normalizing constant?
let normalize: [Float] -> [Float] = lam v. never -- TODO

let normalizeMessage: Message -> Message =
  lam m. map normalize m

-- Elementwise multiplication of state likelihoods/probabilities
let mulMessage: Message -> Message -> Message =
  lam m1. lam m2. never -- TODO

let messageElementPower: Message -> Float -> Message =
  -- TODO
  lam m. never

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

let getAge: MsgTree -> Float = lam tree.

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

type Event = {age: Float, host: Int, from_state: Int, to_state: Int}

type HistoryPoint = {age: Float, repertoire: [Int]}

type HistoryTree
con HistoryLeaf: {
  age: Float,
  label: Label,
  repertoire: [Int],
  history: [HistoryPoint],
} -> HistoryTree
con HistoryNode: {
  age: Float,
  label: Label,
  repertoire: [Int],
  history: [HistoryPoint],
  left: HistoryTree,
  right: HistoryTree,
  debt: Float --TODO Daniel
} -> HistoryTree

------------------------
--- Model parameters ---
------------------------

type ModelParams = {
  q: Matrix Float,
  d_matrix: Matrix Float,
  d_average: Float,
  beta: Float
}


mexpr

-----------------------
--- Model functions ---
-----------------------

recursive let postorder_msgs:
  Tree -> Matrix Int -> Matrix Float -> MsgTree =
    lam tree. lam interactions. lam q.
    match tree with Leaf t then
      MsgLeaf {
        age = 0.,
        label = t.label,
        out_msg = observation_message (get interactions (t.label))
      }
    else match tree with Node t then
      let left = postorder_msgs t.left interactions q in
      let right = postorder_msgs t.right interactions q in

      let leftAge = getMsgTreeAge left in
      let rightAge = getMsgTreeAge right in

      let tLeft =
        transpose
          (matrixExponential (matrixMulScalar (subf t.age leftAge) q)) in
      let tRight =
        transpose
          (matrixExponential (matrixMulScalar (subf t.age rightAge) q)) in

      let left_in_msg  = message (getOutMsg left) tLeft in
      let right_in_msg = message (getOutMsg right) tRight in

      let out_msg = normalizeMessage (mulMessage left_in_msg right_in_msg) in

      MsgNode {
        age=t.age,
        label=t.label,
        left_in_msg=left_in_msg,
        right_in_msg=right_in_msg,
        out_msg=out_msg
      }
    else never
in

recursive let final_probs:
  MsgTree -> Message -> Matrix Float -> Float -> ProbsTree =
    lam tree. lam root_msg. lam q. lam tune.

    let probs: Message =
      normalizeMessage
        (messageElementPower (mulMessage (getOutMsg tree) root_msg) tune) in

    match tree with MsgLeaf t then
      ProbsLeaf { age = 0., label = t.label, probs = probs }
    else match tree with MsgNode t then
      let leftAge = getMsgTreeAge t.left in
      let rightAge = getMsgTreeAge t.right in

      let tLeft = matrixExponential (matrixMulScalar (subf t.age leftAge) q) in
      let tRight = matrixExponential (matrixMulScalar (subf t.age rightAge) q) in

      let left_root_msg =
        message (mulMessage root_msg (t.right_in_msg)) tLeft in
      let right_root_msg =
        message (mulMessage root_msg (t.left_in_msg) tRight in

      let left = final_probs (t.left) left_root_msg q tune in
      let right = final_probs (t.left) right_root_msg q tune in

      ProbsNode { age=t.age, label=t.label, left=left, right=right, probs=probs }
in

let message: Message -> Matrix Float -> Message =
  lam start_msg. lam p.
    -- Assumption: v is a row vector
    map (lam v. matrixMul v p) start_msg
in

let observation_message: Map String Int -> Message =
  lam interactions.
    -- TODO Daniel
    -- msg: Real[][3]
    -- for c in interactions {
    --     msg[i] =
    --         case c == "0" | [1.0, 0.0, 0.0]
    --         case c == "1" | [0.0, 1.0, 0.0]
    --         case c == "2" | [0.0, 0.0, 1.0]
    --         case c == "?" | [1.0, 1.0, 1.0]
    --         // default |  ERROR?
    -- }

    -- return msg

    never
in

let rate: [Int] -> Int -> Int -> ModelParams -> Float =
  lam rep. lam host_index. lam to_state. lam mp.

    let from_state: Int = get rep host_index in

    let base_rate = get (get mp.q from_state) to_state in

    -- TODO Check TreePPL source, update
    if gti from_state to_state then base_rate else

      -- TODO Daniel Filtering operation
      let current_hosts =
        if eqi from_state 0 then
          -- current_hosts = which(rep %in% [1,2])
          never
        else
          -- current_hosts = which(rep == 2)
          never
      in

      -- d = mean (mp.D[host_index][current_hosts])

      -- return base_rate * (exp(-mp.beta*(d/mp.d_average))

      never
in

let total_rate: [Int] -> ModelParams -> Float =
  lam rep. lam mp.

    -- TODO
    -- lossRates =
    --   length(rep==1) -- rep==1 filters rep only for hosts with state 1
    --   *mp.Q[1][0] + length(rep==2)*mp.Q[2][1]

    -- gainRates = 0.0
    -- for (i in 1 to length(rep)) {
    --     if (rep[i] %in% [1,2])
    --         gainRates += rate (rep, i, rep[i]+1, mp)
    -- }

    -- return lossRates + gainRates

    never
in

recursive let simulate_by_event:
  [Int] -> [Event] -> Int -> Float -> Float -> ModelParams -> [HistoryPoint] =
    lam rep. lam events. lam event_index. lam from_age. lam end_age. lam mp.
      let l = length events in
      if gti event_index l then
        let change_rate: Float = total_rate rep mp in
        observe 0 (Poisson (mulf change_rate
                              (subf ((last events).age) end_age)));
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

        -- return cons hp (simulate_by_event (new_rep, events, event_index+1, the_event.age, end_age, mp))

        never
in

let simulate_history:
  HistoryPoint -> HistoryPoint -> ModelParams -> [HistoryPoint] =
    lam from. lam to. lam mp.
      never --TODO Daniel
in

recursive let simulate: ProbsTree -> HistoryPoint -> ModelParams -> HistoryTree =
 lam tree. lam start. lam mp.
   never --TODO Daniel
in

let propose_exponential_max_t: Float -> Float -> Float =
  lam rate. lam max_t.
    never --TODO Daniel
in

recursive let propose_events_for_host:
  Int -> Float -> Float -> Int -> Int -> Matrix Float -> [Event] =
    lam host_index. lam from_age. lam end_age. lam from_state. lam end_state. lam q.
    never --TODO Daniel
in

recursive let propose_events:
  Int -> HistoryPoint -> HistoryPoint -> Matrix Float -> [Event] =
    lam host_index. lam from. lam to. lam q.
      never --TODO Daniel
in

let get_proposal_params:
  Tree -> Matrix Int -> Matrix Float -> [Float] -> Float -> ProbsTree =
    lam parasite_tree. lam interactions. lam q. lam stationary_probs. lam tune.

      let msgTree: MsgTree = postorder_msgs parasite_tree interactions q in

      let pis: Message =
        create (length (getOutMsg msgTree)) (lam. stationary_probs q) in

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
      left = Leaf{age = 0.0, label = "T1"},
      right = Leaf{age = 0.0, label = "T2"},
      label = "index_7"
    },
    right = Node{
      age = 0.153814,
      left = Leaf{age = 0.0, label = "T3"},
      right = Leaf{age = 0.0, label = "T4"},
      label = "index_8"
    },
    label = "index_9"
  },
  right = Node {
    age = 1.079144,
    left = Leaf{age = 0.0, label = "T5"},
    right = Leaf{age = 0.0, label = "T6"},
    label = "index_10"
  },
  label = "index_11"
} in

let interactions: Matrix Char = [
  -- Row labels: T1, T2, T3, T4, T5, T6
  -- Column labels: H1, H2, H3, H4, H5
  ['2','2','0','0','0'],
  ['2','2','0','0','0'],
  ['0','0','2','2','0'],
  ['0','0','2','2','0'],
  ['0','0','0','0','2'],
  ['0','0','0','0','2']
] in

let host_distances: Matrix Float = [
  -- Row and column labels: H1, H2, H3, H4, H5
  [0.,0.8630075756,2.6699063134,2.6699063134,2.6699063134],
  [0.8630075756,0.,2.6699063134,2.6699063134,2.6699063134],
  [2.6699063134,2.6699063134,0.,1.2256551506,1.9598979474],
  [2.6699063134,2.6699063134,1.2256551506,0.,1.9598979474],
  [2.6699063134,2.6699063134,1.9598979474,1.9598979474,0.]
] in

let tune: Float = 0.9 in

-------------------------
--- Model entry point ---
-------------------------
let lambda: [Float] = assume (Dirichlet [1.,1.,1.,1.]) in
let mu: Float = assume (Exponential 10.) in
let beta: Float = assume (Exponential 1.) in

let r: Matrix Float = [
  [negf (get lambda 0), (get lambda 0), 0.],
  [get lambda 1, negf (addf (get lambda 1) (get lambda 2)), get lambda 2],
  [0., get lambda 3, negf (get lambda 3)]
] in

let q: Matrix Float = matrixMulScalar mu r in

-- Hardcoded, should be done in preprocessing --
let n_hosts = 5 in
let d_matrix = host_distances in
let d_average = 4.4 in
------------------------------------------------

let mp: ModelParams =
  { q = q, d_matrix = d_matrix, d_average = d_average, beta = beta } in

-- TODO Daniel. Notes from Fredrik below.
-- Assume 0-indexing.
-- lambda:[Float] -- Contains lambda_01, lambda_10, lambda_12, lambda_21
-- pi:[Float] -- Contains the stationary probs
-- -- You can do this in any order, I start with pi[1]. Sorry for the procedural style...
-- pi[1] = 1. / (1.0 + (lambda[1]/lambda[0]) + (lambda[2]/lambda[3]))
-- pi[0] = pi[1] * (lambda[1]/lambda[0])
-- pi[2] = 1. - pi[0] - pi[1]
--
-- let stationary_probs = ... in

let probs_tree: ProbsTree =
  get_proposal_params parasite_tree interactions q stationary_probs tune in

-- TODO We need to return and store the debts in the root of the history tree
let rep: [(Int)] = create n_hosts (lam i.
  let p = get (getProbs probs_tree) i in
  -- This should use `propose` eventually, now `assume` and `weight` manually
  let v = assume (Categorical p) in
  -- TODO Correct name for pmflogprobcategorical
  let debt = pmfLogProbCategorical p v in
  (v, debt)
) in

(
  if any (eqi 2) rep then
    -- TODO
    -- for i in rep indices:
    --   observe rep[i] ~ Categorical stationary_probs
  else
    weight (negf inf)
);

match probs_tree with ProbsNode n then
  let historyPoint = { age = n.age, repertoire = rep } in
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

  -- TODO Repay _all_ debts here recursively, perhaps stored in the HistoryTree
  -- weight -log_score

  mu
  -- TODO We currently only return mu, but the actual return value should be:
  -- { historyTree = historyTree, lambda = lambda, mu = mu, beta = beta }

else error "Root is not a node!"

