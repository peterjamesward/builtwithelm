module RemoteData exposing (RemoteData(..), map)


type RemoteData a
  = Loading
  | Failure
  | Success a


map : (a -> b) -> RemoteData a -> RemoteData b
map f d =
  case d of
    Loading ->
      Loading

    Failure ->
      Failure

    Success a ->
      Success (f a)
