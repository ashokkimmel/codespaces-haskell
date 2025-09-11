while len(py_warmup_2_q1) < 20:
    py_warmup_2_q1.append(py_warmup_2_q1[-1] + py_warmup_2_q1[-2])
py_warmup_2_q2 = [1]
while len(py_warmup_2_q2) < 20:
    py_warmup_2_q2.append(py_warmup_2_q2[-1] + (len(py_warmup_2_q2)+1) ** 4)

def somewords(lst):
    return filter(lambda s: s > "grapefruit" and len(s) >= 5, lst)
def py_warmup_2_q4 ():
    n = int(input("Give a number: "))
    print(n*20)
def numpair(s):
    lst = s.split()
    answer = []
    j=0
    for i in map(int, lst):
        answer.append((i, j))
        j+=1
    return answer
def large_rem(a,b):
    if (a % 1397) < (b % 1397):
        return b 
    else:
        return a
def myconcat(a,b):
    for x in b:
        a.append(x)
def lastPrint(a):
    while (a != []):
        x = a.pop(-1) 
        print(x)
        if x >= 1000 and a < 10000:
            return a
    return []
py_warmup_2_q9 = []

mdq = collections.deque(np.random.randint(-500, 800,size = 1000))
p = True 
while p and len(mdq) > 0:
    x = mdq.popleft()
    if x > 700:
        p = False 
del p 
for i in range(1, 1001): mdq.appendleft(random.randint(-500, 800))

def howmanybuckets():
    linenum = int(input())
    lstsof0 = list(map(lambda s: 0,range(1000)))
    for i in range(linenum):
        (s,t,b) = map(int,input().split())
        for j in range(s,t+1):
            lstsof0[j] += b
    print(max(lstsof0))

pythag_lst = []
for a in range(3,100):
    for b in range(a, 100): 
        c2 = a ** 2 + b ** 2
        if (math.isqrt(c2) ** 2 == c2):
            pythag_lst.append((a,b, math.isqrt(c2)))

#We did not create duplicates and it is sorted already 
pythag_lst_sorted = pythag_lst
