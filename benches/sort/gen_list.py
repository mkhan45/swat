import random

ls = [random.randint(0, 10_000) for _ in range(5_000)]

with open("list.py", "w") as f:
    f.write(str(ls))

with open("list.ml", "w") as f:
    l_str = "; ".join(map(str, ls))
    f.write(f"[{l_str}]")

with open("list.sax", "w") as f:
    f.write(    f"proc ls (d : list) =\n")
    f.write(    f"cut mt : list\n")
    f.write(    f"    call empty mt\n")
    for (i, elt) in enumerate(ls):
        f.write(f"cut elt{i} : int\n")
        f.write(f"    call _const_{elt} elt{i}\n")
        f.write(f"cut ls{i} : list\n")
        if i == 0:
            f.write(f"    call prepend ls{i} mt elt{i}\n")
        else:
            f.write(f"    call prepend ls{i} ls{i-1} elt{i}\n")
    f.write(    f"id d ls{len(ls) - 1}\n")
