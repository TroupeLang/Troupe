Can you check what happens when 
we import a library, e., A, and then we 
write in the code 

A.bar 

but A doesn't export bar at all. Can you create an example of a program that results in a runtim error? Can you then analyze the complier frontend for how to prevent the runtime error, because in principle the information about the methods in A are available 
in the .exports file. 



