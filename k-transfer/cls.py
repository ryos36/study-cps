k0 = 3
k1 = 4
k2 = 5
#gl = 0

def f0(a0, a1, a2):
    k2 = k0
    k0 = a0 + a1 + a2

    def mask_f(k1, k3):
        def p():
            #print k0 k1 k2 k3
            print k0, k1, k2, k3
        f0_p = p;
    k2 = 10
    def p():
        print k0, k1, k2, k3
    def f1(a0, a1, a2):
        k0 = a0
        k1 = a1
        k2 = a2

    gl = [f0, f1, p]

f0(0, 0, 0)
my_f0 = gl[0]
my_f1 = gl[1]
my_p = gl[1]

my_p()
my_f1(1, 2, 3)
my_p()
my_f0(4, 5, 6)
my_p()
f0_p()

