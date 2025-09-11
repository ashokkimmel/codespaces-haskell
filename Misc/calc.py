var = input("Variable: ")
initial = input("Initial value: ")
fn = input("Iterator: ")
times = int(input("Times: "))
exec(f"{var} = {initial}\nfor _ in range({times}):\n    {var} = {fn}\nprint({var})")    
#
#x=2
#exec("""y = 2
#y + 1""")
#print(x)
#print(y)