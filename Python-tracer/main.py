"""_summary_
"""
import numpy as np
from PIL import Image


HEIGHT = 480
WIDTH = 480


class image():
    def __init__(self, width=WIDTH, height=HEIGHT) -> None:
        self._width = width
        self._height = height
        self._framebuffer = np.empty([self._width * self._height, 3])

    def save(self,) -> None:
        self._framebuffer = self._framebuffer.reshape(
            self._width, self._height, 3)
        im = Image.fromarray((self._framebuffer*255.).astype(np.uint8), 'RGB')
        im.save("out.png")
        im.show()


class Material():
    def __init__(self, refractive_index=1, albedo=np.array([2, 0, 0, 0]), diffuse_color=np.array([0, 0, 0]), specular_exponent=0) -> None:
        self._refractive_index = refractive_index
        self._albedo = albedo
        self._diffuse_color = diffuse_color
        self._specular_exponent = specular_exponent


class sphere():
    def __init__(self, center, radius, material: Material) -> None:
        self._center = center
        self._radius = radius
        self._material = material

ivory = Material(1.0, np.array([0.9,  0.5, 0.1, 0.0]), np.array([0.4, 0.4, 0.3]),   50.)
glass = Material(1.5, np.array([0.0,  0.9, 0.1, 0.8]), np.array([0.6, 0.7, 0.8]),  125.)
red_rubber = Material(1.0, np.array([1.4,  0.3, 0.0, 0.0]), np.array([0.3, 0.1, 0.1]),   10.)
mirror = Material(1.0, np.array([0.0, 16.0, 0.8, 0.0]), np.array([1.0, 1.0, 1.0]), 1425.)


spheres = [
    sphere(np.array([-3,    0,   -16]), 2,      ivory),
    sphere(np.array([-1.0, -1.5, -12]), 2,      glass),
    sphere(np.array([ 1.5, -0.5, -18]), 3, red_rubber),
    sphere(np.array([ 7,    5,   -18]), 4,     mirror)
]

lights = [
    np.array([-20, 20,  20]),
    np.array([ 30, 50, -25]),
    np.array([ 30, 20,  30])
]


def reflect(I: np.array, N: np.array):
    return I - np.dot(N, 2.*np.dot(I, N))

def refract(I: np.array, N: np.array, eta_t: float, eta_i: float=1.):
    cosi = - max(-1., min(1., np.dot(I, N) ))
    if (cosi<0):
        return refract(I, -N, eta_i, eta_t)    
    eta = eta_i / eta_t
    k = 1 - eta*eta*(1 - cosi*cosi)
    if (k<0.):
        return np.array([1., 0., 0.])
    else:
        return I*eta + N*eta*cosi - np.sqrt(k)

def ray_sphere_intersect(orig: np.array, dir: np.array, s: sphere): 
    L = s._center - orig
    tca = np.dot(L, dir)
    d2 = np.dot(L, L) - np.dot(tca, tca)
    if (d2 > s._radius*s._radius):
        return [False, 0]
    thc = np.sqrt(s._radius*s._radius - d2)
    t0 = tca-thc 
    t1 = tca+thc
    if (t0>.001): 
        return [True, t0]
    if (t1>.001):
        return [True, t1]
    return [False, 0]


def scene_intersect(orig: np.array, dir: np.array):

    material = Material()
    nearest_dist = 1e10

    pt = np.array([0.,0.,0.])
    N = np.array([0.,0.,0.])
    
    if (np.abs(dir[1])>.001):
        d = -(orig[1] + 4)/dir[1]
        p = orig + dir * d

        if (d>.001 and d<nearest_dist and np.abs(p[0])<10 and p[2] < -10 and p[2] > -30):
            nearest_dist = d
            pt = p
            N = np.array([0,1,0])
            if((int(0.5*pt[0] +1000) + int(.5*pt[2] )) & 1):
                material._diffuse_color = np.array([.3, .3, .3]) 
            else:
                material._diffuse_color = np.array([.3, .2, .1])

    for s in spheres:
        [intersection, d] = ray_sphere_intersect(orig, dir, s)
        if ( not intersection or d > nearest_dist):
            continue
        nearest_dist = d
        pt = orig + dir*nearest_dist
        N = (pt - s._center) / np.linalg.norm(pt - s._center)
        material = s._material

    return [nearest_dist<1000, pt, N, material]


def cast_ray(orig: np.array, dir: np.array, depth: int=0):

    [hit, point, N, material] = scene_intersect(orig, dir)
    if (depth>4 or not hit):
        return np.array([0.2, 0.7, 0.8])

    reflect_dir = reflect(dir, N) / np.linalg.norm(reflect(dir, N))
    refract_dir = refract(dir, N, material._refractive_index) / np.linalg.norm(refract(dir, N, material._refractive_index))
    reflect_color = cast_ray(point, reflect_dir, depth + 1)
    refract_color = cast_ray(point, refract_dir, depth + 1)

    diffuse_light_intensity = 0
    specular_light_intensity = 0

    for light in lights:
        light_dir = (light - point) / np.linalg.norm(light - point)
        [hit, shadow_pt, trashnrm, trashmat] = scene_intersect(point, light_dir)
        if (hit and np.linalg.norm(shadow_pt-point) < np.linalg.norm(light-point)): 
            continue
        diffuse_light_intensity  += max(0., np.dot(light_dir, N))
        specular_light_intensity += pow(max(0., -np.dot(reflect(-light_dir, N), dir)), material._specular_exponent)

    return material._diffuse_color * diffuse_light_intensity * material._albedo[0] + np.array([1., 1., 1.]) * specular_light_intensity * material._albedo[1] + reflect_color*material._albedo[2] + refract_color*material._albedo[3]






def test_image():
    img = image()
    for j in range(HEIGHT):
        for i in range(WIDTH):
            img._framebuffer[i+j *WIDTH] = np.array([j/float(HEIGHT), i/float(WIDTH), 0.])
    img.save()


if __name__ == "__main__":
    import sys
    fov = 1.05
    img = image()
    
    from tqdm import tqdm
    import multiprocessing as mp
    for pix in tqdm(range(WIDTH * HEIGHT)):
        dir_x =  (pix%WIDTH + 0.5) -  WIDTH/2.
        dir_y = -(pix/WIDTH + 0.5) + HEIGHT/2.
        dir_z = -HEIGHT/(2.*np.tan(fov/2.))
        img._framebuffer[pix] = cast_ray( np.array([0,0,0]), np.array([dir_x, dir_y, dir_z])/np.linalg.norm(np.array([dir_x, dir_y, dir_z])))
