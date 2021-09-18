import re
import os

class converter(object):
    def __init__(self, params):
        self.model = params.get('model')
        self.labels = params.get('labels')
        self.layer_nodes = params.get('layer_nodes')
        self.filename = params.get('filename')

    def export_to_tex(self):
        mod_str = self.model.to_string()
        f = open("figures/%s.dot" %self.filename, "w+")
        f.write(mod_str)
        f.close()
        cmd = "dot2tex -tmath --figonly figures/%s.dot > figures/%s.tex" % (self.filename, self.filename)
        os.system(cmd)
    
    def __construct_labels(self, x_coordinates):
        lines = []
        lines.append("\\begin{scope}\n")
        for l, x in zip(self.labels, x_coordinates):
            lines.append("  \draw (%sbp, -10bp) node {%s};\n" %(x, l))
        lines.append("\\end{scope}\n")
        return lines
    
    def postprocess_tex(self):
        x_coordinates = []
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

            if "\draw" in l and "_{%d}$" %self.layer_nodes[layer] in l:
                layer += 1
                x_coordinates.append(re.search("\([0-9]+.[0-9]", l).group(0)[1:])

            if "\\end{tikzpicture}" in l:
                labels_tex = self.__construct_labels(x_coordinates)
                f.writelines(labels_tex)

            f.write(l)
        f.close()

    def export_processed(self):
        self.export_to_tex()
        self.postprocess_tex()
    

