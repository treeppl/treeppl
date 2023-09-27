#!/usr/bin/env python3

import sys
sys.path.append("..")

import treeppl
import json
from Bio import Phylo
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


class Tree:
    @treeppl.constructor('Node')
    class Node(treeppl.Object):
        def __repr__(self):
            return f"Node(left={self.left!r}, right={self.right!r}, age={self.age})"

    @treeppl.constructor('Leaf')
    class Leaf(treeppl.Object):
        def __repr__(self):
            return f"Leaf(age={self.age})"

    @staticmethod
    def load_nexus(filename):
        tree = Phylo.read(filename, "nexus")
        if not tree.is_bifurcating():
            return
        tree_depths = tree.depths()
        max_age = max(tree_depths.values())
        def convert(clade):
            if clade.is_terminal():
                return Tree.Leaf(
                    age=max_age - tree_depths[clade]
                )
            else:
                return Tree.Node(
                    left=convert(clade.clades[0]),
                    right=convert(clade.clades[1]),
                    age=max_age - tree_depths[clade]
                )
        return convert(tree.root)

    @staticmethod
    def load_phyjson(filename):
        def age(node):
            children = node.get('children')
            if children:
                return max(0., node.get('branch_length', 0)) + max(age(children[0]), age(children[1]))
            return node['branch_length']
        def convert(node, age):
            age -= max(0, node.get('branch_length'))
            children = node.get('children')
            if children:
                node = Tree.Node(
                    left=convert(children[0], age),
                    right=convert(children[1], age),
                    age=age
                )
                return node
            else:
                return Tree.Leaf(age=age)
        phyjson = json.load(open(filename, 'r'))
        return convert(
            phyjson['trees'][0]['root'],
            age(phyjson['trees'][0]['root'])
        )


tree = Tree.load_phyjson("trees/Alcedinidae.phyjson")
rho = 0.5684210526315789

sweep_samples = 10_000
sweep_subsamples = 100
weights = []
samples = []
with treeppl.Model(filename="crbd.tppl", samples=sweep_samples) as crbd:
    while True:
        res = crbd(tree=tree, rho=rho)
        samples.extend(res.subsample(sweep_subsamples))
        weights.extend([res.norm_const]*sweep_subsamples)
        plt.clf()
        sns.kdeplot(x=samples, weights=np.exp(np.array(weights)-max(weights)))
        plt.pause(0.05)
