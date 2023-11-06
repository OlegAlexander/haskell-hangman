module Pics where


showPic :: Int -> String
showPic i = unlines $ pics !! i


pics :: [[String]]
pics =
    [   [ "       ",
          "       ",
          "       ",
          "       ",
          "       ",
          "       ",
          "========="
        ],
        [ "      +",
          "      |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "      |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "      |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          "      |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          "  |   |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|   |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          "      |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          " /    |",
          "      |",
          "========="
        ],
        [ "  +---+",
          "  |   |",
          "  O   |",
          " /|\\  |",
          " / \\  |",
          "      |",
          "========="
        ]
    ]