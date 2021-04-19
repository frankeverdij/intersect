# intersect

This is a FORTRAN90 example program called 'insect' which uses openMP to parallelize brute force searches of segment intersection between two meshes, "red" and "blue"
It outputs the meshes and the intersection in VTU format viewable by Paraview as well as the amount of intersections:

  found       252044  intersections
 
  md5sum red.vtu blue.vtu intersect.vtu
  c61208113fdead669997f19f85ee869b  red.vtu
  80ef5323bb3fe4239fdf7ddc53c99ff0  blue.vtu
  200b1be2ecdc639939b7fcd34acbc488  intersect.vtu

