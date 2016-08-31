(***  Maths tools here ***)

module Math3D = struct

    type vec3D = float*float*float
    
    (* Euclidean norm *)
    let norm x y z =
        sqrt((x*.x)+.(y*.y)+.(z*.z))
        
    let norm3 ((x,y,z) : vec3D) =
        sqrt((x*.x)+.(y*.y)+.(z*.z))
    
    (* scal3ar product *)
    let scal3 ((x1,y1,z1) : vec3D) ((x2,y2,z2) : vec3D) =
        x1*.x2 +. y1*.y2 +. z1*.z2
    
    (* Vectorial product *)
    let cross ((x1,y1,z1) : vec3D) ((x2,y2,z2) : vec3D) =
        ((y1*.z2 -. z1*.y2,
        z1*.x2 -. x1*.z2,
        x1*.y2 -. y1*.x2) : vec3D)
    
    let diff3 ((x1,y1,z1) : vec3D) ((x2,y2,z2) : vec3D) = ((x2-.x1,y2-.y1,z2-.z1) : vec3D)
    
    let fabs x = max (-.x) x
    
    let _pi = 4.*.atan(1.0)
    
    let area a b c =
        let (ab,ac) = (diff3 a b,diff3 a c) in
            0.5*.norm3 (cross ab ac)
            
    let average3 (r1,g1,b1) (r2,g2,b2) (r3,g3,b3) = ((r1+.r2+.r3)/.3.,(g1+.g2+.g3)/.3.,(b1+.b2+.b3)/.3.)
    
end;;