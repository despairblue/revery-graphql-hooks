let initialState = Hashtbl.create(128);
let subscribers = Hashtbl.create(128);

let state = initialState;

let subscribe = (~query: string, subscriberCallback: string => unit) => {
  Printf.printf("Subscribe to %s\n%!", query);
  switch (Hashtbl.find_opt(subscribers, query)) {
  | Some(subscriberCallbacks) =>
    Hashtbl.replace(
      subscribers,
      query,
      [subscriberCallback, ...subscriberCallbacks],
    )
  | None => Hashtbl.add(subscribers, query, [subscriberCallback])
  };

  /* Return unsubscribe-method */
  () =>
    switch (Hashtbl.find_opt(subscribers, query)) {
    | Some(subscriberCallbacks) =>
      let subscribersWithoutCurrentCallback =
        subscriberCallbacks
        |> List.filter(callback => callback !== subscriberCallback);
      Hashtbl.replace(subscribers, query, subscribersWithoutCurrentCallback);
    | None => ()
    };
};

let publish = (~query: string, value: string) => {
  /* Update data in store */
  switch (Hashtbl.find_opt(state, query)) {
  | Some(_data) => Hashtbl.replace(state, query, value)
  | None => Hashtbl.add(state, query, value)
  };

  /* Notify subscribers */
  switch (Hashtbl.find_opt(subscribers, query)) {
  | Some(subscriberCallbacks) =>
    subscriberCallbacks
    |> List.iter(subscriberCallback => subscriberCallback(value))
  | None => Printf.printf("Found no subscribers for %s\n%!", query)
  };
};
