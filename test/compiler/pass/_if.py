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
x_4
print(x_4())
def cond_5():
    if True:
        return "Hello, world!"
    return "hi there!"
print(cond_5())
def outer_6():
    def inner_7(x_8):
        def cond_10():
            if x_8:
                return "true"
            return "false"
        return cond_10()
    inner_7
    return inner_7(True)
outer_6
print(outer_6())
