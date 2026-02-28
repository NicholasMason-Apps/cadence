# apecs-sdl-gdk

`apecs-sdl-gdk` is a lightweight wrapper around Apecs and SDL, providing tools to allow people to get up and running easily with `apecs` and `sdl2` for 2D game development in Haskell. More specifically, the package provides tools to minimise the boilerplate of SDL, and allow users to cleanly do rendering and event handling using a Type system. Additionally, since under the hood the package uses `sdl2`, users are able to freely control the window, renderer, etc. through utilising `sdl2`'s API.

## High-Level Overview

Below is a high-level overview of what `apecs-sdl-gdk` currently provides:
- Easy rendering - through using a Type system and `apecs` Component system, users are able to avoid manually doing rendering calls in SDL by calling a single function to do it for them.
- Streamlined event handling - `apecs-sdl-gdk` exposes a Type to capture any and all polled SDL events, and allow users to easily check for the existance of for manipulation in game logic.

## Links

Below are links to help people get familiar with the package and what it provides, as well as Apecs and SDL:
- [Documentation](URL)
- [Tutorial](URL)
- [Apecs hackage page](https://hackage.haskell.org/package/apecs)
- [SDL2 hackage page](https://hackage.haskell.org/package/sdl2)
- [SDL2 TTF hackage page](https://hackage.haskell.org/package/sdl2-ttf)
- [SDL2 Image hackage page](https://hackage.haskell.org/package/sdl2-image)