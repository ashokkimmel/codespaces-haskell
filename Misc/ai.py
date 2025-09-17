#import math 
#import random
#import collections
#import numpy as np
#
#py_warmup_1_q1 = list(range(1,1000+1))
#
#py_warmup_1_q2 = list(map(lambda s: s * s,range(1,1000+1)))
#py_warmup_1_q3 = 0
#for i in range(1,100+1):
#    py_warmup_1_q3 += i * i * i
#py_warmup_1_q4 = []
#i=0
#while (i*i*i < 20000):
#    if i*i*i > 1000:
#        py_warmup_1_q4 += [i*i*i]
#    i += 1
#del i 
#py_warmup_1_q5 = 0
#for i in py_warmup_1_q4:
#    py_warmup_1_q5 *= i
#py_warmup_1_q6 = math.sin(math.pi/12)
#py_warmup_1_q7 = math.gcd(1558853167347433739,10062552685783700385249011)
#py_warmup_1_q8 = "A dolphin likes the water. A snake slithers in the sand."[-20:]
#py_warmup_1_q9 = "math.pow sets it as a double, while ** works with all types of numbers."
#def py_warmup_1_q10():
#    print("""i carry your heart with me(i carry it in
#my heart)i am never without it(anywhere
#i go you go,my dear;and whatever is done
#by only me is your doing,my darling)
#                                                      i fear""")
#py_warmup_2_q1 = [0, 1]
#while len(py_warmup_2_q1) < 20:
#    py_warmup_2_q1.append(py_warmup_2_q1[-1] + py_warmup_2_q1[-2])
#py_warmup_2_q2 = [1]
#while len(py_warmup_2_q2) < 20:
#    py_warmup_2_q2.append(py_warmup_2_q2[-1] + (len(py_warmup_2_q2)+1) ** 4)
#
#def somewords(lst):
#    return filter(lambda s: s > "grapefruit" and len(s) >= 5, lst)
#def py_warmup_2_q4 ():
#    n = int(input("Give a number: "))
#    print(n*20)
#def numpair(s):
#    lst = s.split()
#    answer = []
#    j=0
#    for i in map(int, lst):
#        answer.append((i, j))
#        j+=1
#    return answer
#def large_rem(a,b):
#    if (a % 1397) < (b % 1397):
#        return b 
#    else:
#        return a
#def myconcat(a,b):
#    for x in b:
#        a.append(x)
#def lastPrint(a):
#    while (a != []):
#        x = a.pop(-1) 
#        print(x)
#        if x >= 1000 and a < 10000:
#            return a
#    return []
#py_warmup_2_q9 = []
#
#mdq = collections.deque(np.random.randint(-500, 800,size = 1000))
#p = True 
#while p and len(mdq) > 0:
#    x = mdq.popleft()
#    if x > 700:
#        p = False 
#del p 
#for i in range(1, 1001): mdq.appendleft(random.randint(-500, 800))
#
#def howmanybuckets():
#    linenum = int(input())
#    lstsof0 = list(map(lambda s: 0,range(1000)))
#    for i in range(linenum):
#        (s,t,b) = map(int,input().split())
#        for j in range(s,t+1):
#            lstsof0[j] += b
#    print(max(lstsof0))
#
#pythag_lst = []
#for a in range(3,100):
#    for b in range(a, 100): 
#        c2 = a ** 2 + b ** 2
#        if (math.isqrt(c2) ** 2 == c2):
#            pythag_lst.append((a,b, math.isqrt(c2)))
#
##We did not create duplicates and it is sorted already 
#pythag_lst_sorted = pythag_lst
#
#py_warmup_3_q1_to_3 = {"red":2,"yellow":10,"green":50}
#py_warmup_3_q1_to_3["blue"] = 90
#py_warmup_3_q1_to_3.pop("red")
#py_warmup_3_q4 = collections.defaultdict(int,py_warmup_3_q1_to_3)
#py_warmup_3_q5 = collections.defaultdict(list,{0:[1,2,3],2:[1,3],3:[1,2]})
#colors = ["Red", "Blue", "Green", "Yellow", "Orange",
#          "Purple", "Pink", "Brown", "Gray", "Teal"]
#def py_warmup_3_colors_q1():
#    print(random.choice(colors))
#py_warmup_3_colors_q2_to_3 = {random.randint(1,1000):random.choice(colors)}
#def do_function_n_times(fun,n):
#    for _ in range(n):
#        fun()
#def setelem(data,index,value):
#    data[index] = value
#do_function_n_times(lambda:setelem(py_warmup_3_colors_q2_to_3,random.randint(1,1000),random.choice(colors)),200)
#def py_warmup_3_colors_q4():
#    do_function_n_times(lambda:print(py_warmup_3_colors_q2_to_4.get(random.randint(1,1000),"transparent")),10)
#py_warmup_3_colors_q5 = collections.defaultdict(int)
#for k,v in py_warmup_3_colors_q2_to_3.items():
#    py_warmup_3_colors_q5[v] += 1
#def py_warmup_3_colors_q6():
#    for k,v in (sorted(py_warmup_3_colors_q5.items(),key=lambda kv: kv[1],reverse=True)): print(f"Color: {k}, Count: {v}")
#
#def py_warmup_3_colors_q7():
#    s = input("What number? ")
#    py_warmup_3_colors_q2_to_3.get(int(s),"never heard of it")
#py_warmup_3_colors_q8 = collections.Counter(py_warmup_3_colors_q2_to_3.values())
#def py_warmup_3_colors_q8_part2():
#    for k,v in (sorted(py_warmup_3_colors_q8.most_common(),key=lambda kv: kv[1],reverse=True)): print(f"Color: {k}, Count: {v}")
#
#x = collections.defaultdict(lambda _: lambda a: a)
import collections
class Graph:
    
    def __init__(self,graphdict,directed = False,nvertices = 0,weights=None):
        self.vertexamount = nvertices
        self.graphdict = graphdict
        self.weights = weights
        if nvertices == 0:
            self.update_nvertices()
        if not directed:
            self.update_undirected_edges()
    def __repr__(self):
        return f"[Graph, V={self.vertexamount}, E={self.graphdict},W ={self.weights}]"
    def update_nvertices(self):
        vertexset = set()
        for k,v in self.graphdict.items():
            vertexset.add(k)
            for i in v:
                vertexset.add(i)
        self.vertexamount = len(vertexset)
    def update_undirected_edges(self):
        newdict = collections.defaultdict(set)
        for k,v in self.graphdict.items():
            newdict[k] = v.copy()
            for i in v:
                if i not in self.graphdict or k not in self.graphdict[i]:
                    newdict[i].add(k)
        self.graphdict = newdict
    
    @classmethod
    def from_file(cls,openfile,directed = False):
        with open(openfile) as readfile:
            lines = readfile.readlines()
            mydict = collections.defaultdict(set)
            foundweighted = False 
            weights = {}
            for i in lines:
                if i.toupper().split() == ["#","WEIGHTS"]:
                    foundweighted = True
                j = i.split('#')[0].split()
                if j:
                    if not foundweighted:
                        x = j.pop(0)
                        mydict[int(x)] = set(map(int,j))
                    else:
                        intj = list(map(int,j))
                        weights[(intj[0],intj[1])] = intj[2] 
            if weights: return Graph(mydict,weights=weights,directed=directed)
            return Graph(mydict,directed=directed)
    
    @classmethod
    def from_str(cls,s,**kwargs):
        
        return Graph()
a = collections.defaultdict(set)
a[0] = {1,2}
print(Graph(a))