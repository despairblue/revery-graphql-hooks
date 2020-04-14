include S;

let useQuery = NewQuery.use;
let useMutation = NewMutation.use;

module Make = (C: BaseConfig) => {
  module Query = {
    module Make = Query.Make(C);
  };

  module Mutation = {
    module Make = Mutation.Make(C);
  };
};
