
infix mem
fun (x mem []) = false
    | (x mem (y::l)) = (x=y) orelse (x mem l);

fun nexts (a,[]) = []
    | nexts (a,(x,y)::pairs) =
        if a = x then y::nexts(a,pairs)
            else nexts(a,pairs);

fun depthf ([], graph, visited) = rev visited
    | depthf (x::xs, graph, visited) =
        if x mem visited then depthf(xs,graph,visited)
            else depthf (nexts(x,graph) @ xs, graph,x::visited);


fun depth ([], graph, visited) = rev visited
    | depth (x::xs, graph, visited) =
        depth (xs,graph,
            if x mem visited then visited
            else depth(nexts(x,graph), graph,x::visited));

fun topsort graph =
    let fun sort ([], visited) = visited
        | sort (x::xs, visited) =
            sort(xs, if x mem visited then visited
                            else x::sort(nexts(x,graph),visited))
        val (starts,_) = ListPair.unzip graph
    in
        sort(starts,[])
    end;

fun newvisit (x,(visited,cys)) = (x::visited, cys);

fun breadth ([], graph, visited) = rev visited
    | breadth (x::xs, graph, visited) =
        let nextnodes = nexts(x,graph)
            fun search [] = visited
                | search (x::xs) =
                    if x mem visited then visited
                    else
                        search(xs)
                        visited = x::visited;

        in
            search



























