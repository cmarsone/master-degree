from statistics import mean
import sys 

import itertools as it
import networkx as nx
import math
import random as rd
from utils import load_steiner, print_graph, eval_sol
from approx_demo import approx_steiner


class Simulated_Annealing() :
    def __init__(self, stein_file:str, p_init:float, p_end:float, nb_iter:int=200):
        self.graph, self.terms = load_steiner(stein_file)
        self.nb_iter = nb_iter

        self.pairs = dict(nx.all_pairs_dijkstra(self.graph))

        #calculate start and end temp
        self.approx_steiner = approx_steiner(self.graph, self.terms)
        # self.T_init = - float(len(self.approx_steiner)) / (3*math.log(p_init))
        # self.T_end = - float(len(self.approx_steiner)) / (3*math.log(p_end))
        self.T_init = - 1. / (math.log(p_init))
        self.T_end = - 1. / (math.log(p_end))


    def energie_E(self, sol:nx.Graph) -> float:
        return eval_sol(self.graph, self.terms, sol.edges())


    def rand_sol(self) -> nx.Graph :
        # sous graph complet sur terms
        complete_graph = nx.complete_graph(self.terms)    

        # arbre couvrant aléatoire
        span = nx.random_spanning_tree(complete_graph)

        res=[]

        # reconstruction de l'arbre couvrant dans le graph
        for node1, node2 in span.edges :
            _, chemins = self.pairs[node1]
            chemin = chemins[node2]

            for i in range(len(chemin) - 1) :
                edge = [chemin[i], chemin[i+1]]

                # si un des sommets de l'arrête est pas dans notres graph, on l'ajoute
                if not all(v in it.chain.from_iterable(res) for v in edge) :
                    res.append(edge)

                # sinon on l'ajoute seulement si les deux sommets ne sont pas dans la meme composante connexe
                elif edge[1] not in nx.node_connected_component(nx.from_edgelist(res), edge[0]):
                    res.append(edge)

        
        # pas ouf mais on peut avoir des feuilles qui ne sont pas dans terms donc on les degage...
        g = nx.from_edgelist(res)
        leafs = [v for v in g.nodes if g.degree(v) == 1]
        leafs_not_terms = [v for v in leafs if not v in self.terms]
        for v in leafs_not_terms : self.rm_useless_from_leaf(g, v)

        return g


    def rand_neighbor(self, sol:nx.Graph) -> nx.Graph:
        new_sol = sol.copy()

        leafs = [x for x in new_sol.nodes if new_sol.degree(x) == 1]
        node1 = rd.choice(leafs)

        #remove useless nodes
        first_not_removed = self.rm_useless_from_leaf(new_sol, node1)

        #chose a random node of the tree to reconnect the leaf        
        node2 = rd.choice([x for x in new_sol.nodes if not x == first_not_removed])
        
        #reconnecting the leaf using djikstra shortest path
        _, chemins = self.pairs[node1]
        chemin = chemins[node2]

        for i in range(len(chemin) - 1) :
            edge = (chemin[i], chemin[i+1])
            if edge[1] in new_sol.nodes : break
            new_sol.add_edge(*edge)

        new_sol.add_edge(*edge) 

        return new_sol


    def simu(self) -> nx.Graph :
        res = self.rand_sol()
        E_res = self.energie_E(res)

        assert self.are_leafs_terms(res),   "rand_sol as leafs that arn't terms !!!"
        assert E_res != -1,                 "rand_sol is not an acceptable sol !"
        
        T = self.T_init
        # T_update = math.pow(self.T_end / self.T_init, 1/(self.nb_iter-1))
        T_update = (self.T_init - self.T_end) / (self.nb_iter - 1)

        for i in range(self.nb_iter) :
            sol = self.rand_neighbor(res)
            E_sol = self.energie_E(sol)

            assert self.are_leafs_terms(sol),   "iter {} : sol as leafs that arn't terms !!!".format(i)
            assert E_sol != -1,                 "iter {} : sol is not an acceptable sol !".format(i)

            if E_sol <= E_res :
                proba = 1
            else : 
                proba = math.exp(- (E_sol - E_res) / T)
                # print("{} : {:.3f}, diff : {}, T : {:.3f}".format(i, proba, E_sol - E_res, T))

            if rd.random() < proba :
                res = sol
                E_res = E_sol
            
            # T *= T_update
            T -= T_update
        
        return res


    #################### utils ####################
    def are_leafs_terms(self, graph:nx.Graph) ->bool :
        leafs = [x for x in graph.nodes if graph.degree(x) == 1]
        if all(v in self.terms for v in leafs) :
            return True
        else :
            return False


    def rm_useless_from_leaf(self, graph:nx.Graph, leaf:int) -> int :
        current = list(graph.neighbors(leaf))[0]
        graph.remove_node(leaf)
        
        while graph.degree(current) == 1 and not current in self.terms :
            next_node = list(graph.neighbors(current))[0]
            graph.remove_node(current)
            current = next_node

        return current
    #################### utils ####################



if __name__ == "__main__":
    rd.seed()

    stein_file = "data/B/b02.stp"
    # stein_file = "data/test.std"
    N = 30
    test = Simulated_Annealing(stein_file, .5, .1, 1000)


    eval_steiner = eval_sol(test.graph, test.terms, test.approx_steiner)
    evals = []
    
    for i in range(N) :
        print(i+1, end="\r")

        sol = test.simu()
        evals.append( eval_sol(test.graph, test.terms, sol.edges) )


    print("min  : {}\nmean : {}\nmax  : {}\nsten : {}".format(min(evals), mean(evals), max(evals), eval_steiner))