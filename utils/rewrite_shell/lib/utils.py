def my_methods(object):
    return [
        method_name
        for method_name in dir(object)
        if callable(getattr(object, method_name))
    ]

def gettypeinfo(tree):
    print(type(tree))
    print("METHODS\n" + str(my_methods(tree)))
    print("FIELDS\n" + str(vars(tree)))
    
def join_long_lines(lines: list[str]) -> list[str]:
    output: list[str] = [""]
    j: int = 0
    concatenating: bool = True
    
    # find long lines and join them
    for line in lines:
        line_no_nl = line.strip()
        # search backward from line end
        indices = list(range(0,len(line_no_nl)))
        indices.reverse()
        if line_no_nl == "":
            j += 1
            output.append("")
            
        for i in indices:
            if not concatenating:
                j += 1
                output.append("")
                
            if line_no_nl[i] == "\\":
                concatenating = True
                keep = line_no_nl[0:i] # remove the backslash
                output[j] += keep
                break # we found the end
            elif line_no_nl[i] == ' ':
                continue
            else:
                concatenating = False
                output[j] += line_no_nl
                break # another end

    # put newlines back
    for i in range(0,len(output)):
        output[i] += "\n"
    
    return output