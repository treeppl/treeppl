include "map.mc"

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

type StateLikelihoods = (Float,Float,Float)

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

-- In TreePPL, we would like to have a conveninent way to refer to the rows and columns of this matrix using names (without runtime overhead)
-- Alternative: type LabeledMatrix row col a = Map (row,col) a
type LabeledMatrix row col a = Map row (Map col a)
let getLM: all row. all col. all a. LabeledMatrix row col a -> row -> col -> a =
 lam row. lam col. lam a. never -- TODO

type Matrix a = [[a]]

let createLM: all row. all col. all a.
               Matrix a -> [row] -> [col] -> LabeledMatrix row col a =
  never

type LabeledStringMatrix a = LabeledMatrix String String a

type ModelParams = {
  q: Matrix Float,
  d_matrix: LabeledStringMatrix Float,
  d_average: Float
}

mexpr

let stationary_probs: Matrix Float -> StateLikelihoods =
  never -- TODO
in

recursive let postorder_msgs:
  Tree -> LabeledStringMatrix Float -> Matrix Float -> MsgTree =
    never -- TODO
in

recursive let final_probs:
  MsgTree -> Message -> Matrix Float -> Float -> ProbsTree =
    never -- TODO
in

let message: Message -> Matrix Float -> Message =
  never -- TODO
in

let observation_message: Map String Int -> Message =
  never -- TODO
in

let rate: [Int] -> Int -> Int -> ModelParams -> Float =
  never --TODO
in

let total_rate: [Int] -> ModelParams -> Float =
  never --TODO
in

recursive let simulate_by_event:
  [Int] -> [Event] -> Int -> Float -> Float -> ModelParams -> [HistoryPoint] =
    never -- TODO
in

let simulate_history:
  HistoryPoint -> HistoryPoint -> ModelParams -> [HistoryPoint] =
    never --TODO
in

recursive let simulate: ProbsTree -> HistoryPoint -> ModelParams -> HistoryTree =
  never --TODO
in

let propose_exponential_max_t: Float -> Float -> Float =
  never --TODO
in


recursive let propose_events_for_host:
  Int -> Float -> Float -> Int -> Int -> Matrix Float =
    never --TODO
in

recursive let propose_events:
  Int -> HistoryPoint -> HistoryPoint -> Matrix Float -> [Event] =
    never --TODO
in

let get_proposal_params:
  Tree -> LabeledStringMatrix Float -> Matrix Float -> Float -> ProbsTree =
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
      [2,2,0,0,0]
      [2,2,0,0,0]
      [0,0,2,2,0]
      [0,0,2,2,0]
      [0,0,0,0,2]
      [0,0,0,0,2]
    ] ["T1","T2","T3","T4","T5","T6"] ["H1","H2","H3","H4","H5"] in

let host_distances: LabeledStringMatrix Float =
  createLM [
      [0.           , 0.8630075756 , 2.6699063134 , 2.6699063134 , 2.6699063134],
      [0.8630075756 , 0.           , 2.6699063134 , 2.6699063134 , 2.6699063134],
      [2.6699063134 , 2.6699063134 , 0.           , 1.2256551506 , 1.9598979474],
      [2.6699063134 , 2.6699063134 , 1.2256551506 , 0.           , 1.9598979474],
      [2.6699063134 , 2.6699063134 , 1.9598979474 , 1.9598979474 , 0.]
    ] ["H1","H2","H3","H4","H5"] ["H1","H2","H3","H4","H5"] in

let tune: Real = 0.9 in

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

let tot_dist = 0. in
let n_hosts = 5 in
let d_matrix = host_distances in
let d_average = 4.4 in

let mp: ModelParams =
  { q = q, d_matrix = host_distances, d_average = d_average } in

mu


