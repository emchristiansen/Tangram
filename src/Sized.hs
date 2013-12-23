
-- An element associated with a 2D size.
-- For example, this could be an image associated with a particular resize
-- target.
data Sized a = Sized {
  _sizedSizeL :: Size,
  _sizedElementL :: a
}
makeFields ''Sized2D
