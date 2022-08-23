# funtracer
`funtracer` is a simple functional raytracer written in Haskell.

# Running
To run the project on the default scene use the command `stack run`.
Currently no other scenes are available.

# Output images
![Cornell Box](renders/CornellBox.png?raw=true "Cornell Box")

# Features / TODO list
- [x] Direct lighting model
- [ ] Blinn–Phong reflection model 
- [ ] Refractions 
- [ ] Parsing scene from file
- [ ] Loading `.obj` files
- [ ] Acceleration structures (KD-Tree)
- [ ] Interactive preview

# Libraries used
- [Linear](https://hackage.haskell.org/package/linear) for linear algebra 
- [Gloss](https://hackage.haskell.org/package/gloss) for drawing output