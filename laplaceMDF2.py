#MDF eletrostatico Gauss-Seidel e loop vetorizado em Isolver
#Author:Agora sou eu
#License: nenhuma


import numpy as np
import matplotlib.pyplot as plt
import time


class Grid: 
    """Nesta classe sao armazenados os detalhes da malha"""
    def __init__(self, nx, ny, xmin, xmax,ymin, ymax):
        self.xmin, self.xmax, self.ymin, self.ymax = xmin, xmax, ymin, ymax
        dx = float(xmax-xmin)/(nx-1)
        dy = float(ymax-ymin)/(ny-1)
        self.x = np.arange(xmin, xmax + dx*0.5, dx)
        self.y = np.arange(ymin, ymax + dy*0.5,dy)
        self.u = np.zeros((nx, ny),float)
        self.old_u = self.u.copy()        
        self.nl=0
        self.nr=0
        self.nb=0
        self.nt=0

    def setBC(self, l, r, b, t,nl,nr,nb,nt):        
        """Impoe as condicoes de contorno de Dirichlet"""        
        self.u[:,0] = l
        self.u[:,-1] = r
        self.u[0, :] = b
        self.u[-1,:] = t
        self.old_u = self.u.copy()        
        self.nl=nl
        self.nr=nr
        self.nb=nb
        self.nt=nt
        
        
    def Return(self):
        return self.x,self.y

class LaplaceSolver:
    
    """Solver iterativo para o Laplaciano com vetorializacao da varredura de u"""
    
    def __init__(self, grid):
        self.grid = grid

    def Isolver(self):
       g = self.grid
       u = g.u
       g.old_u = u.copy()
       nl=g.nl
       nr=g.nr
      
       u[1:-1,0]= nl*u[1:-1,3]
       u[1:-1,ny-1]=nr*u[1:-1,ny-3]
       u[1:-1, 1:-1] =(((u[0:-2, 1:-1] + u[2:, 1:-1]) + 
                         (u[1:-1,0:-2] + u[1:-1, 2:]))*0.25)
       v = (g.u - g.old_u).flat
       err=np.sqrt(np.dot(v,v))
       return err
        
    def Return(self):
        return self.grid.u
                    
nx=20
ny=20         
eps=1.0e-10
ierr=1.0
n_iter=4000
count = 1
m=Grid(nx, ny, xmin=0.0, xmax=10.0,
                 ymin=0.0, ymax=10.0)
m.setBC(0,0.,0.,100.,0.,0.,0.,0.)
x ,y=m.Return()
m1=LaplaceSolver(m)
t1=time.clock()
while  ierr > eps:
    if count > n_iter:break
    ierr=m1.Isolver()
    count = count + 1
dt=time.clock()-t1
print "Solution for nx = ny = %d, %d iteractions, took %f seconds"%(nx,count,dt)
X,Y = np.meshgrid(x, y)
msol=m1.Return()
plt.contour(X,Y, msol,15,linewidth=.3)
plt.contourf(X,Y, msol, 15,alpha=0.9,cmap=plt.cm.rainbow)
plt.title('Contour Plot')
plt.xlabel('x (cm)')
plt.ylabel('y (cm)')
plt.show()
plt.colorbar()
