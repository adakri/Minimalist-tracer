import Codec.Picture (Image, PixelRGBA8 (..), generateImage, imageHeight, imageWidth, writePng)
import Data.Maybe (fromJust)

-- A point in 3D space
data Point = Point {x :: Float, y :: Float, z :: Float}

-- A sphere defined by its center and radius
data Sphere = Sphere {center :: Point, radius :: Float}

-- A ray defined by its origin and direction
data Ray = Ray {origin :: Point, direction :: Point}

-- Calculate the intersection point of a ray and a sphere
intersect :: Ray -> Sphere -> Maybe Point
intersect (Ray (Point ox oy oz) (Point dx dy dz)) (Sphere (Point cx cy cz) r) =
  let a = dx * dx + dy * dy + dz * dz
      b = 2 * (dx * (ox - cx) + dy * (oy - cy) + dz * (oz - cz))
      c = (ox - cx) * (ox - cx) + (oy - cy) * (oy - cy) + (oz - cz) * (oz - cz) - r * r
      discriminant = b * b - 4 * a * c
   in if discriminant < 0
        then Nothing
        else
          let t1 = (- b + sqrt discriminant) / (2 * a)
              t2 = (- b - sqrt discriminant) / (2 * a)
              t = if t1 > 0 then t1 else t2
           in Just (Point (ox + t * dx) (oy + t * dy) (oz + t * dz))

-- Render the scene by tracing rays from the eye through each pixel
render :: Int -> Int -> Point -> [Sphere] -> [[(Int, Int, Int)]]
render w h eye spheres =
  [[traceRay (Ray eye (Point (fromIntegral x - fromIntegral w / 2) (fromIntegral y - fromIntegral h / 2) (fromIntegral h / 2))) spheres | x <- [0 .. w -1]] | y <- [0 .. h -1]]

-- Trace a single ray through the scene, returning the color of the closest intersection
traceRay :: Ray -> [Sphere] -> (Int, Int, Int)
traceRay ray spheres =
  case intersectRay ray spheres of
    Nothing -> (0, 0, 0)
    Just (Sphere _ r) -> (floor (r * 255), floor (r * 255), floor (r * 255))

-- Find the intersection of a ray with any of the spheres in the scene
intersectRay :: Ray -> [Sphere] -> Maybe Sphere
intersectRay ray spheres =
  foldr
    ( \sphere closest -> case closest of
        Nothing -> case intersect ray sphere of
          Nothing -> Nothing
          Just _ -> Just sphere
        Just _ -> closest
    )
    Nothing
    spheres

main :: IO ()
main = do
  let w = 800
      h = 600
      eye = Point 0 0 (-1)
      sphere = Sphere (Point 0 0 1) 1
      imageData = render w h eye [sphere]
      image = generateImage pixelRenderer w h
  writePng "output.png" image
  where
    pixelRenderer x y = let (r, g, b) = imageData !! y !! x in PixelRGBA8 r g b 255
