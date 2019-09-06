#### About
A drag and drop reorderable list.
Use the grey handle to move the list items.

![What it looks like](/example.gif?raw=true "What it looks like")

#### Purpose
This repo isn't so much a library as it is an example of two useful patterns:
1. Using the css transform() property to limit browser layout calls
2. Creating an ocaml functor that is parameterized by a runtime environment (JSOO in this case)

#### Issues
The following issues are present, but can be fixed with a little bit of elbow grease
- Only supports vertical orientation
- Each cell has a fixed height
- Only one draggable item at a time
- Difficult to customize CSS

#### Dependencies
I use python SimpleHTTPServer as a local file server
It is compiled using ocaml 4.07.0 and dune 1.11.3
