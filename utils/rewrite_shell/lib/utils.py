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