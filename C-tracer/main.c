#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include <stdbool.h>

int   width  = 720;
int   height = 720;
float fov    = 1.05;
#define bg_colour (vec3){0.2, 0.7, 0.8}


// --------------------------------------------------------------------------------------
// 3D vector struct

struct vec3 {
    float x, y, z;
};
typedef struct vec3 vec3;
// Vector operators
vec3  k_vec(const float k, const vec3 v) { return (vec3){.x=v.x*k, .y=v.y*k, .z=v.z*k};}

float vec_vec(const vec3 u, const vec3 v) { return u.x*v.x + u.y*v.y + u.z*v.z; }

vec3  vecPlusvec(const vec3 u, const vec3 v) { return (vec3){u.x+v.x, u.y+v.y, u.z+v.z}; }

vec3  vecMinusvec(const vec3 u, const vec3 v) { return (vec3){u.x-v.x, u.y-v.y, u.z-v.z}; }

vec3  MinusVec(const vec3 u) { return (vec3){-u.x, -u.y, -u.z};}

float norm(const vec3 u){ return sqrt(u.x*u.x+u.y*u.y+u.z*u.z); }

vec3 normalize(const vec3 u){ return k_vec(1./norm(u), u); }

vec3 cross(const vec3 v1, const vec3 v2) {
    return (vec3){ v1.y*v2.z - v1.z*v2.y, v1.z*v2.x - v1.x*v2.z, v1.x*v2.y - v1.y*v2.x };
}

// --------------------------------------------------------------------------------------
// Sphere struct

struct sphere{
    vec3 center;
    float radius;
};
typedef struct sphere sphere;

sphere* sphere_construct(vec3 center, float radius)
{
    sphere* obj = malloc(sizeof(sphere));
    obj->center = center;
    obj->radius = radius;
    return obj; 
}

bool ray_intersect(const sphere s, const vec3 orig, const vec3 dir, float t0) 
{
    vec3 L = vecMinusvec(s.center, orig);
    float tca = vec_vec(L, dir);
    float d2 = vec_vec(L, L) - tca*tca;
    if (d2 > s.radius*s.radius) return false;
    float thc = sqrtf(s.radius*s.radius - d2);
    t0 = tca - thc;
    float t1 = tca + thc;
    if (t0 < 0) t0 = t1;
    if (t0 < 0) return false;
    return true;
}

vec3 cast_ray(const vec3 orig, const vec3 dir, const sphere s) {
    float sphere_dist = FLT_MAX;
    if (!ray_intersect(s, orig, dir, sphere_dist)) {
        return bg_colour; // background color
    }
    return (vec3){0.4, 0.4, 0.3};
}


// --------------------------------------------------------------------------------------
// Rendering
void render_frameBuffer(const int width, const int height, vec3 *frameBuffer, char* name)
{
    FILE *fptr;
    // create filename
    char filename[sizeof("images/")/sizeof(char) + sizeof(name)/sizeof(char)];
    strcpy(filename, "images/");
    strcat(filename, name);

    fptr = fopen(filename,"w");
    fprintf(fptr,"P6\n %d %d \n255\n",width, height);

    for (size_t i = 0; i < height*width; ++i) {
        fprintf(fptr, "%c%c%c", (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].x))), 
                                  (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].y))),
                                  (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].z))));
    }
    fclose(fptr);
}



void render(const int width, const int height, const sphere s, vec3 *frameBuffer) {
    for (size_t j = 0; j<height; j++) {
        for (size_t i = 0; i<width; i++) {
            float x =  (2*(i + 0.5)/(float)width  - 1)*tan(fov/2.)*width/(float)height;
            float y = -(2*(j + 0.5)/(float)height - 1)*tan(fov/2.);
            vec3 dir = normalize( (vec3){x, y, -1} );
            frameBuffer[i+j*width] = cast_ray((vec3){0,0,0}, dir, s);
        }
    }
}

// --------------------------------------------------------------------------------------
// Tests

void render_frameBuffer_test()
{
    vec3 frameBuffer[width*height];
    // construct toy frame buffer
    for(size_t i=0; i<height; i++)
    {
        for(size_t j=0; j<width; j++)
        {
            frameBuffer[j+i*width] = (vec3){i/(float)height, j/(float)width, 0.};
        }
    }
    render_frameBuffer(width, height, frameBuffer, "test.ppm");
}

void ray_cast_test()
{
    vec3 frameBuffer[width*height];
    // construct toy frame buffer
    for(size_t i=0; i<height; i++)
    {
        for(size_t j=0; j<width; j++)
        {
            frameBuffer[j+i*width] = (vec3){i/(float)height, j/(float)width, 0.};
        }
    }
    vec3 origin = (vec3){0., 0., 0.};
    sphere* s = sphere_construct(origin, 0.01);
    render(width, height, *s, frameBuffer);
    render_frameBuffer(width, height, frameBuffer, "test.ppm");
}


int main()
{
    //render_frameBuffer_test();
    ray_cast_test();
    
    return 0;
}