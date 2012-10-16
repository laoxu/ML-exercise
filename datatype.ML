
datatype person = King
                | Peer of string*string*int
                | Knight of string
                | Peasant of string;

fun title King = "his majesty the King"
    | title (Peer(deg,terr,_)) = "the " ^ deg ^ " of " ^ terr
    | title (Knight name) = "sir " ^ name
    | title (Peasant name) = name;

fun sirs [] = []
    | sirs ((Knight s) :: ps) = s :: (sirs ps)
    | sirs (p :: ps) = sirs ps;

datatype degree = Duke | Marquis | Earl | Viscount | Baron;



