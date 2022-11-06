#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// 3D vector struct
struct vec3 {
    float x, y, z;
};
typedef struct vec3 vec3;
// Vector operators
vec3  k_vec(const float k, const vec3 v) { return (vec3){.x=v.x*k, .y=v.y*k, .z=v.z*k};}

float vec_vec(const vec3 u, const vec3 v) { return u.x*v.x + u.y*v.y + u.z*v.z; }

vec3  vecPlusvec(const vec3 u, const vec3 v) { return (vec3){u.x+v.x, u.y+v.y, u.z+v.z}; }

vec3  MinusVec(const vec3 u) { return (vec3){-u.x, -u.y, -u.z};}

float norm(const vec3 u){ return sqrt(u.x*u.x+u.y*u.y+u.z*u.z); }

vec3 normalized(const vec3 u){ return k_vec(1./norm(u), u); }




// Render test
void render_frameBuffer(const int width, const int height, vec3 *frameBuffer, char* name)
{
    FILE *fptr;
    fptr = fopen("images/test.ppm","w");
    fprintf(fptr,"P6\n %d %d \n255\n",width, height);

    for (size_t i = 0; i < height*width; ++i) {
        fprintf(fptr, "%c%c%c", (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].x))), 
                                  (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].y))),
                                  (char)(255 * fmax(0.f, fmin(1.f, frameBuffer[i].z))));
    }
    fclose(fptr);
}

void render_frameBuffer_test()
{
    int width = 720, height=720;
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


int main()
{
    render_frameBuffer_test();
    
    
    return 0;
}