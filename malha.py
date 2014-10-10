# -*- coding: utf-8 -*-
"""
Created on Wed Oct 08 12014

@author: MV
"""
import sys
import desenha_malha as ds
import matplotlib.pyplot as plt

try:
    infilename=sys.argv[1]
except:
    print "Usage:", sys.argv[0], "infile"; sys.exit(1)

infilename1=infilename+".malha"

#arquivo .malha
m = ds.Mesh(infilename1)
m.plotMesh()
plt.show()
