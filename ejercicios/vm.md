C(ifz cond then t else e) = C(cond) ; IFZ (t; e) 

IFZ(t, e) := Pop_Jump_if_not_0 length(t) + 1; C(t), jump length(e); C(e)   

-- Aca ya esta compilado 

<Pop_Jump_if_not_0 n ; k | e | 0:s> -> <k[n:] | e | s> 
<Pop_Jump_if_not_0 n ; k | e | 1:s> -> <k | e | s>

<jump n; k | e | s> -> <k[n:] | e | s>
