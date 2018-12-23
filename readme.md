#### About
A drag and drop reorderable list
Use the grey handle to move the list items

![What it looks like](/example.gif?raw=true "What it looks like")

#### Nice things
It runs fast because any per-frame rendering is done on a 'transform()' css property (does not trigger a dom layout)

#### Issues
Only vertical orientation is supported at the moment
It is also a fixed height per cell, but that can be expanded with more JS integration in the future
Can only drag one item at a time
It is very hard to edit the css and customize it, but it can be refactored to make it easier
