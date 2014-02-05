 let execution_time func times=
   let rec helper n utime_acc stime_acc =
     if n = times then  [utime_acc; stime_acc]
     else       
       let t1 = Unix.times () in
       let result = func () in     
       let t2 = Unix.times () in
       let utime = t2.Unix.tms_utime -. t1.Unix.tms_utime in
       let stime = t2.Unix.tms_stime -. t1.Unix.tms_stime in
       let utime_acc = utime_acc +. utime in
       let stime_acc = stime_acc +. stime in
       helper (n+1) utime_acc stime_acc
   in
   match (helper 1  0. 0.) with
     | [utime_acc; stime_acc] ->        
       let t = float_of_int times in
       Printf.printf "utime=%f, stime=%f, sum=%f\n" (utime_acc/.t) (stime_acc/.t) ((utime_acc +. stime_acc)/.t);
