module RemoteData exposing (RemoteData(..), withDefault, map)


type RemoteData a
  = Loading
  | Failure
  | Success a


withDefault : a -> RemoteData a -> a
withDefault default d =
  case d of
    Loading ->
      default

    Failure ->
      default

    Success a ->
      a


map : (a -> b) -> RemoteData a -> RemoteData b
map f d =
  case d of
    Loading ->
      Loading

    Failure ->
      Failure

    Success a ->
      Success (f a)
