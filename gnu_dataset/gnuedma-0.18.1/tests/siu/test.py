id = edma.new_obj ("PERSON")
print ("-- Running Display method on Class PERSON from Python! ------\n");
edma.met3 (id, "display", "");
print ("\n");
print ("-- Running REALIZATION>met2 on Class PERSON from Python! ----\n");
edma.met3 (id, "REALIZATION>met2", "s", ("Hello from a script",));
edma.free_obj (id);
