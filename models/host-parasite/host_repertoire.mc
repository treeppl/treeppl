include "map.mc"
include "math.mc"

type Tree
con Leaf: {
  age: Float,
  label: String
} -> Tree
con Node: {
  age: Float,
  left: Tree,
  right: Tree,
  label: String
} -> Tree

-- Always three elements, but not expressed in type
type StateLikelihoods = [Float]

type Message = [StateLikelihoods]

type MsgTree
con MsgLeaf: {
  age: Float,
  label: String,
  out_msg: Message
} -> MsgTree
con MsgNode: {
  age: Float,
  label: String,
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

type ProbsTree
con ProbsLeaf: {
  age: Float,
  label: String,
  probs: Message
} -> ProbsTree
con ProbsNode: {
  age: Float,
  label: String,
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
  label: String,
  repertoire: [Int],
  history: [HistoryPoint]
} -> HistoryTree
con HistoryNode: {
  age: Float,
  label: String,
  repertoire: [Int],
  history: [HistoryPoint],
  left: HistoryTree,
  right: HistoryTree
} -> HistoryTree

-- In TreePPL, we would like to have a conveninent way to refer to the rows and
-- columns of this matrix using names (without runtime overhead)
-- Alternative: type LabeledMatrix row col a = Map (row,col) a
type LabeledMatrix row col a = Map row (Map col a)
let getLM: all row. all col. all a.
  LabeledMatrix row col a -> row -> col -> a =
    lam row. lam col. lam a.
      mapFindExn col (mapFindExn row a)


-- Matrix stuff, we need to implement this (in the Miking standard library). We
-- should probably do it for tensors instead (and also add support for tensors
-- in CorePPL)
type Matrix a = [[a]]

let transpose: Matrix Float -> Matrix Float =
  lam m. never -- TODO

let matrixExponential: Matrix Float -> Matrix Float =
  lam m. never -- TODO

let matrixMulScalar: Matrix Float -> Float -> Matrix Float =
  lam m. lam s. never -- TODO

let matrixMulElement: Matrix Float -> Matrix Float -> Matrix Float =
  lam m1. lam m2. never -- TODO

let createLM: all row. all col. all a.
               Matrix a -> [row] -> [col] -> LabeledMatrix row col a =
  never

-- TODO Discuss: do we actually need the labels in the TreePPL code? Can we not verify things in pre- and post processing steps?
type LabeledStringMatrix a = LabeledMatrix String String a

type ModelParams = {
  q: Matrix Float,
  d_matrix: LabeledStringMatrix Float,
  d_average: Float,
  beta: Float
}

mexpr

let stationary_probs: Matrix Float -> StateLikelihoods =
  lam q.
  never -- TODO Fredrik Mariana
in

recursive let postorder_msgs:
  Tree -> LabeledStringMatrix Int -> Matrix Float -> MsgTree =
    lam tree. lam interactions. lam q.
    match tree with Leaf t then
      MsgLeaf{
        age = 0.,
        label = t.label,
        out_msg = observation_message (mapFindExn (t.label) interactions)
      }
    else match tree with Node t then
      let left = postorder_msgs t.left interactions q in
      let right = postorder_msgs t.right interactions q in

      let tLeft = transpose (matrixExponential (matrixMulScalar q (subf t.age left.age))) in
      let tRight = transpose (matrixExponential (matrixMulScalar q (subf t.age right.age))) in

      -- left_in_msg  = message( left.out_msg, Tleft)
      -- right_in_msg = message(right.out_msg, Tright)

      -- TODO Check if correct?
      let out_msg = matrixMulElement left_in_msg right_in_msg in

      -- return MsgNode(age=tree.age, label=tree.label, left_in_msg=left_in_msg, right_in_msg=right_in_msg, out_msg=out_msg)
      never
    else never
in

recursive let final_probs:
  MsgTree -> Message -> Matrix Float -> Float -> ProbsTree =
    lam tree. lam root_msg. lam q. lam tune.

    -- TODO Fredrik: What operation is ^?
    -- let probs = (matrixMulElement tree.out_msg root_msg)^tune in

    -- if tree is Leaf
    --     return ProbsLeaf(age=0.0, label=tree.label, probs=probs)

    -- Tleft  = exp(Q*(tree.age-tree.left.age))
    -- Tright = exp(Q*(tree.age-tree.right.age))

    -- TODO Fredrik: * unclear. We think it's elementwise?
    -- left_root_msg  = message(root_msg*tree.right_in_msg, Tleft)
    -- right_root_msg = message(root_msg*tree.left_in_msg , Tright)

    -- left  =  final_probs(tree.left,  left_up_msg, Q, tune)
    -- right =  final_probs(tree.left, right_up_msg, Q, tune)

    -- return ProbsNode(age=tree.age, label=tree.label, left=left, right=right, probs=probs)

    never
in

let message: Message -> Matrix Float -> Message =
  lam start_msg. lam t.
    -- TODO Fredrik: * operation elementwise or standard matrixmul?
    -- for (i=1 to lengths(start_msg)[1])
    --     end_msg[i] = start_msg[i]*T

    -- return end_msg
    never
in

let observation_message: Map String Int -> Message =
  lam interactions.
    -- msg: Real[][3]
    -- for c in interactions {
    --     msg[i] =
    --         -- TODO Daniel: Match on values, not labels
    --         case c == "0" | [1.0, 0.0, 0.0]
    --         case c == "1" | [0.0, 1.0, 0.0]
    --         case c == "2" | [0.0, 0.0, 1.0]
    --         -- TODO Encode ? as -1 for now. Discuss later.
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

    if gti from_state to_state then base_rate else

      -- TODO Filtering operation
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

    -- for (i in 1 to length(rep)) -- TODO Remove for loop? i never used
    --     lossRates =
    --       length(rep==1) -- rep==1 filters rep only for hosts with state 1
    --       *mp.Q[1][0] + length(rep==2)*mp.Q[2][1]

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
    -- TODO What is normalize?
    never --TODO Daniel
in

recursive let propose_events:
  Int -> HistoryPoint -> HistoryPoint -> Matrix Float -> [Event] =
    lam host_index. lam from. lam to. lam q.
      never --TODO Daniel
in

let get_proposal_params:
  Tree -> LabeledStringMatrix Int -> Matrix Float -> Float -> ProbsTree =
    lam parasite_tree. lam interactions. lam q. lam tune.

    let msgTree: MsgTree = postorder_msgs parasite_tree interactions q in

    let pis: Message =
      create (length (getOutMsg msgTree)) (lam. stationary_probs q) in

    final_probs msgTree pis q tune

in

-- Data
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

-- let mapFromSeq : all k. all v. (k -> k -> Int) -> [(k, v)] -> Map k v =

let interactions: LabeledStringMatrix Int =
  createLM [
      [2,2,0,0,0],
      [2,2,0,0,0],
      [0,0,2,2,0],
      [0,0,2,2,0],
      [0,0,0,0,2],
      [0,0,0,0,2]
    ] ["T1","T2","T3","T4","T5","T6"] ["H1","H2","H3","H4","H5"] in

-- TODO Change to matrix
let host_distances: LabeledStringMatrix Float =
  createLM [
      [0.           , 0.8630075756 , 2.6699063134 , 2.6699063134 , 2.6699063134],
      [0.8630075756 , 0.           , 2.6699063134 , 2.6699063134 , 2.6699063134],
      [2.6699063134 , 2.6699063134 , 0.           , 1.2256551506 , 1.9598979474],
      [2.6699063134 , 2.6699063134 , 1.2256551506 , 0.           , 1.9598979474],
      [2.6699063134 , 2.6699063134 , 1.9598979474 , 1.9598979474 , 0.]
    ] ["H1","H2","H3","H4","H5"] ["H1","H2","H3","H4","H5"] in

let tune: Float = 0.9 in

-- Model entry point
let lambda: [Float] = assume (Dirichlet [1.,1.,1.,1.]) in
let mu: Float = assume (Exponential 10.) in
let beta: Float = assume (Exponential 1.) in

let r: Matrix Float = [
  [negf (get lambda 0), (get lambda 0), 0.],
  [get lambda 1, negf (addf (get lambda 1) (get lambda 2)), get lambda 2],
  [0., get lambda 3, negf (get lambda 3)]
] in

let q: Matrix Float = map (lam row. map (lam e. mulf mu e) row) r in

---- Hardcoded for now ----
let n_hosts = 5 in
let d_matrix = host_distances in
let d_average = 4.4 in
---------------------------

let mp: ModelParams =
  { q = q, d_matrix = d_matrix, d_average = d_average, beta = beta } in

let probs_tree: ProbsTree = get_proposal_params parasite_tree interactions q tune in

-- This should use `propose` eventually, now `assume` and `weight` manually
let rep: [Int] = create n_hosts (lam i.
  let p = get (getProbs probs_tree) i in
  assume (Categorical p)
) in

(
  if any (eqi 2) rep then
    weight 0. -- TODO Mariana Fredrik
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

  -- TODO We currently only return mu
  -- { historyTree = historyTree, lambda = lambda, mu = mu, beta = beta }
  mu

else error "Root is not a node!"

