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
// Lights

struct light {
    vec3 position;
    float intensity;
};
typedef struct light light;

light* light_construct(const vec3 p, const float i)
{
    light* obj = malloc(sizeof(light));
    obj->position = p;
    obj->intensity = i;
    return obj;
}


// --------------------------------------------------------------------------------------
// Materials
struct material{
    vec3 diffuse_colour;
};
typedef struct material material;

material* material_construct(vec3 colour)
{
    material* obj= malloc(sizeof(material));
    obj->diffuse_colour = colour;
    return obj;
}

// Examples of materials
const vec3 mat1 = (vec3){0.4, 0.4, 0.3}; //ivory
const vec3 mat2 = (vec3){0.3, 0.1, 0.1}; // wood


// --------------------------------------------------------------------------------------
// Sphere struct

struct sphere{
    vec3 center;
    float radius;
    material mat;
};
typedef struct sphere sphere;

sphere* sphere_construct(vec3 center, float radius, material mat)
{
    sphere* obj = malloc(sizeof(sphere));
    obj->center = center;
    obj->radius = radius;
    obj->mat = mat;
    return obj; 
}

void print_sphere(sphere s)
{
    printf("=============================================\n");
    printf("center %f, %f, %f \n", s.center.x, s.center.y, s.center.z);
    printf("radius %f \n", s.radius);
    printf("material %f, %f, %f \n", s.mat.diffuse_colour.x, s.mat.diffuse_colour.y, s.mat.diffuse_colour.z);
    printf("=============================================\n");
}

// --------------------------------------------------------------------------------------
// Ray intersection and geometry
bool ray_intersect(const sphere s, const vec3 orig, const vec3 dir, float* t0) 
{
    vec3 L = vecMinusvec(s.center, orig);
    float tca = vec_vec(L, dir);
    float d2 = vec_vec(L, L) - tca*tca;
    if (d2 > s.radius*s.radius) return false;
    float thc = sqrtf(s.radius*s.radius - d2);
    *t0 = tca - thc;
    float t1 = tca + thc;

    if (t0 < 0) *t0 = t1;
    if (t0 < 0) return false;
    return true;
}

bool scene_intersect(const vec3 orig, const vec3 dir, const sphere* spheres, const int number_of_spheres, vec3* hit, vec3* N, material* mat) {
    float spheres_dist = FLT_MAX;
    for (size_t i=0; i < number_of_spheres; i++) {
        float dist_i = FLT_MAX;
        if (ray_intersect(spheres[i], orig, dir, &dist_i) && dist_i < spheres_dist) {
            spheres_dist = dist_i;
            *hit = vecPlusvec(orig , k_vec(dist_i, dir));
            *N = normalize((vecMinusvec(*hit, spheres[i].center)));
            *mat = spheres[i].mat;
        }
    }
    return spheres_dist<1000;
}

/*
vec3 cast_ray(const vec3 orig, const vec3 dir, const sphere s) {
    float sphere_dist = FLT_MAX;
    if (!ray_intersect(s, orig, dir, &sphere_dist)) {
        return bg_colour; // background color
    }
    return (vec3){0.4, 0.4, 0.3};
}
*/

vec3 cast_ray(const vec3 orig, const vec3 dir, const sphere* spheres, const int number_of_spheres, const light* l, const int number_of_lights) {
    vec3 point;
    vec3 N;
    material mat;
    if (!scene_intersect(orig, dir, spheres, number_of_spheres, &point, &N, &mat)) {
        return bg_colour; // background color
    }

    // Add lighting
    float diffuse_light_intensity = 0.;

    for (size_t i=0; i<number_of_lights; i++) {
        vec3 light_dir = normalize(vecMinusvec(l[i].position, point));
        diffuse_light_intensity  += l[i].intensity * fmax(0.f, vec_vec(light_dir, N));
    }

    return k_vec(diffuse_light_intensity, mat.diffuse_colour);
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



void render(const int width, const int height, const sphere* spheres, const int number_of_spheres, light* l, const int number_of_lights, vec3 *frameBuffer) {

    for (size_t j = 0; j<height; j++) {
        for (size_t i = 0; i<width; i++) {
            float x =  (2*(i + 0.5)/(float)width  - 1)*tan(fov/2.)*width/(float)height;
            float y = -(2*(j + 0.5)/(float)height - 1)*tan(fov/2.);
            vec3 dir = normalize( (vec3){x, y, -1} );
            frameBuffer[i+j*width] = cast_ray((vec3){0,0,0}, dir, spheres, number_of_spheres, l, number_of_lights);
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
    // Define materials
    material* ivory = material_construct( mat1 );
    material* redRubber = material_construct( mat2 );

    // construct toy frame buffer
    vec3 frameBuffer[width*height];
    for(size_t j=0; j<height; j++)
    {
        for(size_t i=0; i<width; i++)
        {
            frameBuffer[i+j*width] = (vec3){i/(float)height, j/(float)width, 0.};
        }
    }
    // Define spheres
    vec3 origin1 = (vec3){-3, 0, -16};
    vec3 origin2 = (vec3){-1.0, -1.5, -12};
    vec3 origin3 = (vec3){1.5, -0.5, -18};
    vec3 origin4 = (vec3){7,    5,   -18};

    sphere spheres[4];
    spheres[0] = *sphere_construct(origin1, 2, *ivory);
    spheres[1] = *sphere_construct(origin2, 2, *redRubber);
    spheres[2] = *sphere_construct(origin3, 3, *redRubber);
    spheres[3] = *sphere_construct(origin4, 4, *ivory);

    int number_of_spheres = sizeof(spheres)/sizeof(sphere);

    // Define light
    light L[1];
    vec3 position = (vec3){-20, 20, 20}; 
    float intensity = 1.5;

    L[0] = *light_construct(position, intensity);

    int number_of_lights = sizeof(L)/sizeof(light);


    render(width, height, spheres, number_of_spheres, L, number_of_lights, frameBuffer);
    render_frameBuffer(width, height, frameBuffer, "test.ppm");
}




int main()
{
    //render_frameBuffer_test();
    ray_cast_test();
    
    return 0;
}