open Revery.UI.React;

type status('a) =
  | Idle
  | Loading
  | Error
  | Data('a);

type action('a) =
  | Fetch
  | Error
  | Data('a);

let stringOfAction =
  fun
  | Fetch => "Fetch"
  | Error => "Error"
  | Data(_) => "Data";

let reducer = (action, _state): status('a) => {
  Printf.printf("dispatched: %s\n%!", stringOfAction(action));
  switch (action) {
  | Fetch => Loading
  | Error => Error
  | Data(obj) => Data(obj)
  };
};

let initialState: status('a) = Idle;

let use =
    (
      ~definition,
      ~config: S.config,
      ~variables: option(Yojson.Basic.t)=?,
      (),
    ) => {
  let (parse, query, composeVariables) = definition;
  let%hook (state, dispatch) = Hooks.reducer(~initialState, reducer);
  let mergedConfig = config;

  let query = () => {
    let variables =
      switch (variables) {
      | Some(variables) => variables
      | None => `Assoc([])
      };

    let query =
      `Assoc([("query", `String(query)), ("variables", variables)])
      |> Yojson.Basic.to_string;

    (query, variables);
  };

  let tap = (~l=?, ~f=ignore, x) => {
    Option.iter(l => Printf.printf("%s: ", l), l);
    f(x);
    Printf.printf("\n%!");
    x;
  };

  let subscribeToStore = query =>
    Store.subscribe(
      ~query,
      graphqlJson => {
        let data =
          try(
            graphqlJson
            |> tap(~l="graphqlJson", ~f=ignore)
            |> Yojson.Basic.from_string
            |> tap(~l="fromString", ~f=ignore)
            |> Yojson.Basic.Util.member("data")
            |> tap(~l="member", ~f=ignore)
            |> tap(~l="data", ~f=data =>
                 Printf.printf("%s", Yojson.Basic.to_string(data))
               )
            |> parse
          ) {
          | firstException =>
            Printf.printf("This Crashed\n%!");
            raise(Not_found);
          };

        dispatch(Data(data));
      },
    );

  let executeRequest = (query, variables) => {
    dispatch(Fetch);

    Printf.printf("Fetching\n%!");
    Fetch.(
      post(
        ~body=query,
        ~headers=[
          ("Content-Type", "application/json"),
          ...mergedConfig.headers,
        ],
        mergedConfig.baseUrl,
      )
      |> Lwt.map(result => {
           Printf.printf("Got result\n%!");

           switch (result) {
           | Ok({Response.body, _}) =>
             let query =
               `Assoc([
                 ("query", `String(query)),
                 ("variables", variables),
               ])
               |> Yojson.Basic.to_string;

             Printf.printf("%s\n%!", Body.toString(body));
             Store.publish(~query, Body.toString(body));
           | _ => dispatch(Error)
           };
         })
    )
    |> ignore;
  };

  /* TODO: use OnMountAndIf when Revery has been updated to latest brisk */
  let%hook () =
    Hooks.effect(
      OnMount,
      () => {
        let (query, variables) = query();
        let unsubscribe = subscribeToStore(query);

        executeRequest(query, variables);

        Some(unsubscribe);
      },
    );

  let%hook () =
    Hooks.effect(
      If(
        (prevVariables, nextVariables) => prevVariables != nextVariables,
        variables,
      ),
      () => {
        let (query, variables) = query();
        let unsubscribe = subscribeToStore(query);
        executeRequest(query, variables);

        Some(unsubscribe);
      },
    );

  state;
};
