def move(n, start, to, spare):
    if n == 1:
        print("Move {} to {}".format(start, to))    # the move for n
    else:
        #import pdb; pdb.set_trace()
        move(n-1, start, spare, to)                 # move all rings on top of n to spare
        print("Move {} to {}".format(start, to))    # guaranteed to be valid
        move(n-1, spare, to, start)                 # move rings back to the top of n

def move_iter(n, start, to, spare, ):
    if n == 1:
        print("Move {} to {}".format(start, to))
    else:
        print("Move {} to {}".format(start, to))
        return move_iter(n-1, start, spare, to, True)

def move_iter(n, start, to, spare, spare_full):
    return move_iter(n-1, spare, to, start)
    
move(3, 1, 3, 2)
print("-------")
move_iter(3, 1, 3, 2)
