def cond_0():
    if True:
        return 42
    return 41
cond_0()
def anon_1():
    return "hello"
def anon_2():
    return "bye"
def cond_3():
    if True:
        return anon_1
    return anon_2
x_4 = cond_3()
print(x_4())
def cond_5():
    if True:
        return "Hello, world!"
    return "hi there!"
print(cond_5())
