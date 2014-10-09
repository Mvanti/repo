#!MDF eletrostatico com sobrerelaxacao e loop convencional de indices em Isolver
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
        self.nl=nl
        self.nr=nr
        self.nb=nb
        self.nt=nt
        
        
    def Return(self):
        return self.x,self.y

class LaplaceSolver:
    
    """Solver iterativo para o Laplaciano."""
    
    def __init__(self, grid):
        self.grid = grid

    def Isolver(self, alfa):
        g = self.grid
        nx,ny = g.u.shape
        u = g.u
        nl=g.nl
        nr=g.nr

        err = 0.0
        for i in range(1, nx-1):
            for j in range(1, ny-1):
                tmp = u[i,j]
                u[i,0]= nl*u[i,3]
                u[i,ny-1]=nr*u[i,ny-3]
                r=-4.0*u[i,j]+ (u[i-1, j] + u[i+1, j]+u[i, j-1] + u[i, j+1])
                u[i,j] = u[i,j]+0.25*alfa*r
                diff = u[i,j] - tmp
                err += np.sqrt(diff*diff)
        return err
        
    def Return(self):
        return self.grid.u
                    
nx=20
ny=20        
eps=1.0e-10
ierr=1.0
n_iter=2000
count = 1
alfa=1.72
m=Grid(nx, ny, xmin=0.0, xmax=10.0,
                 ymin=0.0, ymax=10.0)
m.setBC(0,0.,0.,100.0,0.,0.,0.,0.)
x ,y=m.Return()
m1=LaplaceSolver(m)
t1=time.clock()
while  ierr > eps:
    if count > n_iter:break
    ierr=m1.Isolver(alfa)
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
