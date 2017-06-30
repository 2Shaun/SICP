def move(n, start, to, spare):
    if n == 1:
        print("Move {} to {}".format(start, to))    # the move for n
    else:
        #import pdb; pdb.set_trace()
        move(n-1, start, spare, to)                 # move all rings on top of n to spare
        move(n-1, spare, to, start)                 # move rings back to the top of n
        print("Move {} to {}".format(start, to))    # guaranteed to be valid

def move_iter(n, start, to, spare):
    
    
move(3, 1, 3, 2)
