import re
import os

class converter(object):
    def __init__(self, params):
        self.figure = params.get('figure')
        self.labelled_nodes = params.get('labelled_nodes')
        self.filename = params.get('filename')

    def export_to_tex(self):
        mod_str = self.figure.to_string()
        f = open("figures/%s.dot" %self.filename, "w+")
        f.write(mod_str)
        f.close()
        cmd = "dot2tex -tmath --figonly figures/%s.dot > figures/%s.tex" % (self.filename, self.filename)
        os.system(cmd)
    
    def __construct_labels(self):
        lines = []
        lines.append("\\begin{scope}\n")
        for l in self.labelled_nodes.values():
            lines.append("  \draw (%sbp, -10bp) node {%s};\n" %(l[1], l[0]))
        lines.append("\\end{scope}\n")
        return lines
    
    def postprocess_tex(self):
        f = open("figures/" + self.filename + ".tex")
        lines = f.readlines()
        f.close()
        layer = 0

        f = open("figures/" + self.filename + ".tex", "w+")
        for l in lines:
            if "\\begin{tikz" in l:
                l = "\\begin{tikzpicture}[>=latex,line join=bevel, scale = 0.2]\n"

            if "\pgfsetlinewidth{1bp}" in l:
                l = "  \pgfsetlinewidth{0.2bp}\n"

            if "\draw" in l and any("_{%d}$" %k in l for k in self.labelled_nodes.keys()):
                node = int(re.search("\_\{[0-9]+", l).group(0)[2:])
                x_coordinate = re.search("\([0-9]+.[0-9]", l).group(0)[1:]
                self.labelled_nodes[node] =  (self.labelled_nodes[node], x_coordinate)

            if "\\end{tikzpicture}" in l:
                labels_tex = self.__construct_labels()
                f.writelines(labels_tex)

            f.write(l)
        f.close()

    def export_processed(self):
        self.export_to_tex()
        self.postprocess_tex()
    

