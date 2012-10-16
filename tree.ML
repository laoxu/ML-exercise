
datatype 'a tree = Lf
                    | Br of 'a * 'a tree * 'a tree;

structure Tree =
    struct
    fun size Lf = 0
        | size (Br(v,t1,t2)) = 1 + size t1 + size t2;

    fun depth Lf = 0
        | depth (Br(v,t1,t2)) = 1+ Int.max(depth t1, depth t2);

    fun preorder Lf = []
        | preorder (Br(v,t1,t2)) = [v] @ preorder t1 @ preorder t2;

    fun inorder Lf = []
        | inorder (Br(v,t1,t2)) = inorder t1 @ [v] @ inorder t2;

    fun postorder Lf = []
        | postorder (Br(v,t1,t2)) = postorder t2 @ postorder t1 @ [v];

    fun balpre [] = Lf
        | balpre (x::xs) =
            let val k = length xs div 2
            in Br(x,balpre(List.take(xs,k)), balpre(List.drop(xs,k)))
            end;
    fun balin [] = Lf
        | balin xs =
            let val k = length xs div 2
                val y::ys = List.drop(xs,k)
            in Br(y,balin(List.take(xs,k)),balin ys)
            end;

end;
















