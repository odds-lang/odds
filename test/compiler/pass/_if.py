def _cond_0():
    if True:
        return 42
    return 41
_cond_0()
def _anon_1():
    return "hello"
def _anon_2():
    return "bye"
def _cond_3():
    if True:
        return _anon_1
    return _anon_2
x_4 = _cond_3()
x_4
print(x_4())
def _cond_5():
    if True:
        return "Hello, world!"
    return "hi there!"
print(_cond_5())
def outer_6():
    def inner_7(x_8):
        def _cond_10():
            if x_8:
                return "true"
            return "false"
        return _cond_10()
    inner_7
    return inner_7(True)
outer_6
print(outer_6())
