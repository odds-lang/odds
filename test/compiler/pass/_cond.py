def anon_0():
    return "hello"
def anon_1():
    return "bye"
def cond_2():
    if True:
        return anon_0
    else:
        return anon_1

x = cond_2()
