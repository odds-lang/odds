def anon_0():
    return "hello"
def anon_1():
    return "bye"
def cond_2():
    if True:
        return anon_0
    return anon_1
x = cond_2()
print(x())
def cond_3():
    if True:
        return "Hello, world!"
    return "hi there!"
print(cond_3())
