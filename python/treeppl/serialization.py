import json
import numpy as np

from .exceptions import SerializationError


constructor_to_class = {}
class_to_constructor = {}


class Object:
    def __init__(self, **kwargs):
        for key, value in kwargs.items():
            setattr(self, key, value)


def object_hook(dictionary):
    if "__constructor__" in dictionary:
        constructor = dictionary["__constructor__"]
        if constructor not in constructor_to_class:
            constructor_to_class[constructor] = type(constructor, (Object,), {})
            class_to_constructor[constructor_to_class[constructor]] = constructor
        return constructor_to_class[constructor](**dictionary.get("__data__", {}))
    if "__float__" in dictionary:
        return float(dictionary["__float__"])
    if "__tensor__" in dictionary:
        return np.array(dictionary["__tensor__"]).reshape(dictionary["__tensorShape__"])
    return dictionary


def constructor(name):
    def wrapper(cls):
        constructor_to_class[name] = cls
        class_to_constructor[cls] = name
        return cls
    return wrapper


def from_json(fp):
    return json.load(fp, object_hook=object_hook)


class JSONEncoder(json.JSONEncoder):
    def default(self, obj):
        try:
            if isinstance(obj, np.ndarray):
                return {
                    "__tensor__": obj.flatten().tolist(),
                    "__tensorShape__": obj.shape
                }
            return json.JSONEncoder.default(self, obj)
        except TypeError:
            try:
                return {
                    "__constructor__": class_to_constructor.get(
                        obj.__class__,
                        obj.__class__.__name__
                    ),
                    "__data__": vars(obj)
                }
            except:
                raise SerializationError("Could not serialize the data.")


def to_json(value, fp):
    return json.dump(value, fp, cls=JSONEncoder)
