# Minimalis-tracer

This is mainly for self-reference.

## Preambule

This project has started as me trying to find a little insignificant yet cool project to be a little hands on with Haskell: I really liked the language but have seen that I spent most of my time (most of it on [leran you a haskell](http://learnyouahaskell.com/)) doing mostly reading and barely doing any coding. So, coming from a computer graphics/HPC background, I thought that coding some sort of minimalist ray tracer (like the one in [this amazing CG course by D.Sokolov](https://github.com/ssloy/tinyrenderer/wiki) ) would be a good idea. So I satrted by implementing it in Python! Very counter-intuitive, but I needed a prototype scene to compare my output to and the easiest way was to do it in Python (using only Numpy and PIL, so not so minimaist after all). However, the impelemntation was very slow (maybe I'll look into that someday!) so I  coded it in C (now using nothing! so we're going into the direction of minimalism but still no Haskell). Now that the impelemntation is pretty good and that I have a pretty good grasp of what I should do (took me long enough!), now onto Haskell!

**Main aim**: Use Haskell Multi-threading/GPU interfaces to accelearte the application.

## Scene

![alt text](out.png "Ray tarced scene")

## Specifications for Python

You'll need [PIL](https://pillow.readthedocs.io/) and [Numpy](https://numpy.org/):

```shell
pip install Pillow numpy
```

## Specifications for Haskell
You'll need [Juicy Pixels](https://hackage.haskell.org/package/JuicyPixels) to run the haskell code.
Here are the steps you can follow to install JuicyPixels:

```shell
cabal --version
```
If cabal is not installed, you can install it by following the instructions on [the Haskell Platform website](https://www.haskell.org/platform/).

Update the package list by running the following command:
```shell
cabal update
cabal install JuicyPixels
```
## Specification in C

In a truly minimalist approach, you only need [the gcc compiler](https://gcc.gnu.org/) or equivalent.