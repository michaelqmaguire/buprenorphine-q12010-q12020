if (dir.exists("./data")) {
  print("Directory already exists!")
} else {
  dir.create("./data")
}

if (dir.exists("./data/clean/")) {
  print("Directory already exists!")
} else {
  dir.create("./data/clean/")
}

if (dir.exists("./data/raw/")) {
  print("Directory already exists!")
} else {
  dir.create("./data/raw/")
}

if (dir.exists("./output")) {
  print("Directory already exists!")
} else {
  dir.create("./output")
}

if (dir.exists("./functions")) {
  print("Directory already exists!")
} else {
  dir.create("./functions")
}

if (dir.exists("./plots")) {
  print("Directory already exists!")
} else {
  dir.create("./plots")
}