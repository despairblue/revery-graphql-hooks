type config = {
  baseUrl: string,
  headers: list((string, string)),
};

module type BaseConfig = {let config: config;};

module type MutationConfig = {
  let query: string;

  type t;
  let parse: Yojson.Basic.t => t;
};

module type QueryConfig = {
  let query: string;

  type t;
  let parse: Yojson.Basic.t => t;
};
