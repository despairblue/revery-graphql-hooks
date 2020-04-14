open Revery.UI.React;

include S;

module type Query = {
  type t;

  type status =
    | Idle
    | Loading
    | Error
    | Data(t);

  let use:
    (
      ~config: S.config=?,
      ~variables: Yojson.Basic.t=?,
      unit,
      Hooks.t(
        (
          Hooks.Reducer.t(status),
          Hooks.Effect.t(Hooks.Effect.onMount),
          Hooks.Effect.t(option(Yojson.Basic.t))
        ) =>
        'a,
        'b,
      )
    ) =>
    (status, Hooks.t('a, 'b));
};

module Make = (C: BaseConfig, G: QueryConfig) : (Query with type t = G.t) => {
  type t = G.t;
  let baseConfig = C.config;

  type status =
    | Idle
    | Loading
    | Error
    | Data(G.t);

  type action =
    | Fetch
    | Error
    | Data(G.t);

  let stringOfAction =
    fun
    | Fetch => "Fetch"
    | Error => "Error"
    | Data(_) => "Data";

  let reducer = (action, _state): status => {
    switch (action) {
    | Fetch => Loading
    | Error => Error
    | Data(obj) => Data(obj)
    };
  };

  let initialState: status = Idle;

  let use =
      (
        ~config: option(S.config)=?,
        ~variables: option(Yojson.Basic.t)=?,
        (),
      ) => {
    let%hook (state, dispatch) = Hooks.reducer(~initialState, reducer);
    let mergedConfig = Option.value(config, ~default=baseConfig);

    let query = () => {
      let variables =
        switch (variables) {
        | Some(variables) => variables
        | None => `Assoc([])
        };

      let query =
        `Assoc([("query", `String(G.query)), ("variables", variables)])
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
              |> G.parse
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
                   ("query", `String(G.query)),
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
};
