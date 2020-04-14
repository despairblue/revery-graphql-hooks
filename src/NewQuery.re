open Revery.UI.React;

let tap = Utils.tap;
let createBody = Utils.createBody;

type t('responseType) = 'responseType;

type status('responseType) =
  | Idle
  | Loading
  | Error
  | Data(t('responseType));

type action('responseType) =
  | FetchData
  | Error
  | Data('responseType);

let stringOfAction =
  fun
  | FetchData => "FetchData"
  | Error => "Error"
  | Data(_) => "Data";

let reducer = (action, _state): status('responseType) => {
  Printf.printf("New Query: dispatched: %s\n%!", stringOfAction(action));
  switch (action) {
  | FetchData => Loading
  | Error => Error
  | Data(obj) => Data(obj)
  };
};

let initialState: status('responseType) = Idle;

let use =
    (
      ~variables: option(Yojson.Basic.t)=?,
      ~onCompleted=ignore,
      ~definition,
      ~config: S.config,
      (),
    ) => {
  let (parse, query, composeVariables) = definition;

  let parseResponse = graphqlJson =>
    try(
      graphqlJson
      |> tap(~l="Subscriber", ~f=ignore)
      |> Yojson.Basic.from_string
      |> tap(~l="Yojson.Basic.from_string", ~f=ignore)
      |> Yojson.Basic.Util.member("data")
      // |> tap(~l={|Yojson.Basic.Util.member("data")|}, ~f=data =>
      //      Printf.printf("%s", Yojson.Basic.to_string(data))
      //    )
      |> parse
      |> tap(~l="parse", ~f=ignore)
      |> (data => Ok(data))
    ) {
    | firstException => Error(Not_found)
    };

  let%hook (state, dispatch) = Hooks.reducer(~initialState, reducer);

  let subscribeToStore = query =>
    Store.subscribe(
      ~query,
      graphqlJson => {
        let data = parseResponse(graphqlJson);

        switch (data) {
        | Ok(data) => dispatch(Data(data))
        | Error(error) =>
          Printf.printf("This Crashed\n%!");
          dispatch(Error);
        };
      },
    );

  let executeRequest = (payload, variables) =>
    if (state != Loading) {
      dispatch(FetchData);
      Fetch.(
        post(
          ~body=payload,
          ~headers=[("Content-Type", "application/json"), ...config.headers],
          config.baseUrl,
        )
        |> Lwt.map(result => {
             Printf.printf("Got result\n%!");

             switch (result) {
             | Ok({Response.body, _}) =>
               let responseBody = Body.toString(body);
               let result = parseResponse(responseBody);

               onCompleted(result);
               Store.publish(~query=payload, responseBody);
             | _ => dispatch(Error)
             };
           })
      )
      |> ignore;
    };

  let rerender = () => {
    let (payload, variables) = createBody(~query, ~variables?);

    let unsubscribe = subscribeToStore(payload);
    executeRequest(payload, variables);

    Some(unsubscribe);
  };

  let%hook () =
    Hooks.effect(
      OnMountAndIf((previous, next) => previous != next, variables),
      rerender,
    );
  let%hook () =
    Hooks.effect(
      OnMountAndIf((previous, next) => previous !== next, definition),
      rerender,
    );
  let%hook () =
    Hooks.effect(
      OnMountAndIf((previous, next) => previous != next, config.headers),
      rerender,
    );
  let%hook () =
    Hooks.effect(
      OnMountAndIf((previous, next) => previous != next, config.baseUrl),
      rerender,
    );

  state;
};
