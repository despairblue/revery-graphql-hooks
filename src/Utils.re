let tap = (~l=?, ~f=ignore, x) => {
  Option.iter(l => Printf.printf("%s: ", l), l);
  f(x);
  Printf.printf("\n%!");
  x;
};

let createBody = (~variables=`Assoc([]), ~query) => {
  let payload =
    `Assoc([("query", `String(query)), ("variables", variables)])
    |> Yojson.Basic.to_string;

  (payload, variables);
};
