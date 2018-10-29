open Core
open Stdio

type bump = Major | Minor | Patch

let parse_version line =
  try
    let numbers = String.split ~on:'.' line |> List.map ~f:Int.of_string in
    match numbers with
    | [major; minor; patch] -> Some (major, minor, patch)
    | _ -> None
  with _ -> None

let lowercase_and_strip = Fn.compose String.strip String.lowercase

let parse_command s =
  match lowercase_and_strip s with
  | "major" -> Some Major
  | "minor" -> Some Minor
  | "patch" -> Some Patch
  | _ -> None

let print_usage () =
  print_endline
    "Usage: bump_version expects two lines on stdin. The first is the current \
     version, and the second is a command, which must be one of 'major', \
     'minor', and 'patch'. The updated version is output on stdout."

let print_version (major, minor, patch) = printf "%i.%i.%i\n" major minor patch

let () =
  let open Option in
  let version = In_channel.input_line In_channel.stdin >>= parse_version in
  let command = In_channel.input_line In_channel.stdin >>= parse_command in
  match (version, command) with
  | None, _ | _, None -> print_usage () ; exit 1
  | Some (major, _, _), Some Major -> print_version (major + 1, 0, 0)
  | Some (major, minor, _), Some Minor -> print_version (major, minor + 1, 0)
  | Some (major, minor, patch), Some Patch ->
      print_version (major, minor, patch + 1)
