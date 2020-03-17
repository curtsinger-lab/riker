Naive Build Algorithm
---------------------
run(Command root):
  # Mark all commands as undone
  initialize(root)
  
  # Create a work queue with the root command
  queue = [root]

  # Loop as long as there is work to do
  while queue is not empty:
    # Grab the first command from the queue
    c = take(queue)
    
    # We are going to decide two things:
    #   Have this commands inputs been produced so we can decide whether or not to run it, and
    #   have any of the inputs to this command changed, necessitating a run?
    ready = true
    must_run = false
  
    # Loop over inputs
    for i in c.inputs:
      # If an artifact version was created by a command we haven't visited yet, c is not ready
      if i.creator != None and i.creator.done == false:
        ready = false
      
      # If an input to c has changed, c must run
      else if changed(i):
        must_run = true
    
    # If this command isn't ready, stick it on the back of the queue
    if !ready:
      queue.append(c)
    
    # If the command has to run, we'll run it
    else if must_run:
      ...
    
    # If the command does not need to run, we can emulate it
    else:
      # Mark the command as done, which makes its outputs available to other commands
      c.done = true
      
      # Add this command's children to the work queue
      for child in c.children:
        queue.append(child)
      
      
    
initialize(Command c):
  c.done = false
  for child in c.children:
    initialize(child)

