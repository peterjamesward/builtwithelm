module Data exposing (Project, projects)


type alias Project =
  { name : String
  , description : String
  , primaryUrl : String
  , previewImageUrl : String
  , repositoryUrl : Maybe String
  }


projects : List Project
projects =
  [ { previewImageUrl = "data/images/elmify.png"
    , name = "Elmify - stats about your listening tastes"
    , primaryUrl = "https://elmify.netlify.app/"
    , description = "Display your top tracks and top artists based on your listening habits on Spotify. Discover which songs suit your listening tastes the best."
    , repositoryUrl = Just "https://github.com/jindrazak/Elmify"
    }
  , { previewImageUrl = "data/images/BFG_SpeedMath.png"
    , name = "Brain Floss Games | Speed Math"
    , primaryUrl = "http://brainflossgames.com/"
    , description = "With these digit-by-digit instructions and unlimited practice questions, learning speed math has never been easier."
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/unli-rev-flight.jpg"
    , name = "UNLI Reverse Flight Search"
    , primaryUrl = "https://unli.xyz/flights/"
    , description = "Search flights in reverse. Made in 10 days from zero Elm experience to deployment with elm-sortable-table, kirchner/elm-selectize, and ohanhi/remotedata-http"
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/fourseasons.png"
    , name = "Across the Four Seasons"
    , primaryUrl = "https://fourseasons.aularon.com"
    , description = "An exploration into Vivaldi's Four Seasons concerti"
    , repositoryUrl = Just "https://github.com/aularon/four-seasons"
    }
  , { previewImageUrl = "data/images/swshdex.png"
    , name = "Sword/Shield Pokedex - A Pokemon Sword/Shield Helper App"
    , primaryUrl = "https://dex.3digit.dev"
    , description = "Multi-function helper app built for Sword/Shield -- Type Matchups, Party Planner, and more"
    , repositoryUrl = Just "https://github.com/3digitdev/swshdex"
    }
  , { previewImageUrl = "data/images/noconf.png"
    , name = "NoConf - conference agenda builder application"
    , primaryUrl = "https://noconf.io"
    , description = "A.I. based conference scheduler with organizer dashboard built fully with Elm"
    , repositoryUrl = Nothing
    }
  , { previewImageUrl = "data/images/fretmaster.jpg"
    , name = "FretMaster"
    , primaryUrl = "https://fretmaster.app/"
    , description = "Guitar fretboard learning tool / game"
    , repositoryUrl = Just "https://github.com/dkarter/fretmaster-elm"
    }
  , { previewImageUrl = "data/images/odyssey.png"
    , name = "Odyssey"
    , primaryUrl = "https://odyssey.neophilus.net/"
    , description = "Next generation gallery. Exceptional images deserve an exceptional presentation."
    , repositoryUrl = Just "https://github.com/Libbum/Odyssey"
    }
  , { previewImageUrl = "data/images/SlimeBuddyScreenshot_1000x800.png"
    , name = "Slime Buddy"
    , primaryUrl = "https://slime-buddy.netlify.com/"
    , description = "A little buddy for your desktop"
    , repositoryUrl = Just "https://github.com/wolfadex/slime-buddy"
    }
  , { previewImageUrl = "data/images/dodge.png"
    , name = "Dodge"
    , primaryUrl = "https://wolfadex.github.io/dodge/"
    , description = "A simple doding game"
    , repositoryUrl = Just "https://github.com/wolfadex/dodge/"
    }
  , { previewImageUrl = "data/images/verwonderwereld.jpg"
    , name = "Naturalis Verwonderwereld"
    , primaryUrl = "https://www.verwonderpaspoort.nl/verwonderwereld"
    , description = "Interactive world for Dutch science museum aimed at elementary school students. Made by Driebit Amsterdam"
    , repositoryUrl = Nothing
    }
  ]
