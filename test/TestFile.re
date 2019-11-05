open TestFramework;
open Library;

let connection_url = "postgresql://postgres:postgres@localhost:5432/db_name";
let pool = Client.create_pool(~size=8, ~connection_url, ());

module UsersTable = {
  type t = {
    id: string,
    first: string,
    last: string,
  };

  let tname = "authors";

  let columns =
    QueryBuilder.[
      Table.Col.make(
        ~key="id",
        ~encode=t => string(t.id),
        ~primary_key=true,
        ~unique=true,
        (),
      ),
      Table.Col.make(~key="first", ~encode=t => string(t.first), ()),
      Table.Col.make(~key="last", ~encode=t => string(t.last), ()),
    ];
};

module UsersTableImpl = Table.Make(UsersTable);

let select_all = () => {
  QueryBuilder.(
    select(
      [e(col("id")), e(col("first")), e(col("last"))]
      |> from(table(UsersTableImpl.table_name)),
    )
  );
};

describe("QueryBuilder", ({test}) => {
  let Ok(_) =
    pool
    |> Client.drop_table(~table_name=UsersTableImpl.table_name)
    |> Lwt_main.run;
  let Ok(_) =
    pool
    |> Client.create_table(~query=UsersTableImpl.create_table())
    |> Lwt_main.run;

  test("CREATE TABLE", ({expect}) => {
    let create_table_query =
      Sql.CreateTable(UsersTableImpl.create_table()) |> Postgres.render;
    expect.string(create_table_query).toMatchSnapshot();
  });

  test("SELECT", ({expect}) => {
    let select_table_query = Sql.Select(select_all()) |> Postgres.render;
    expect.string(select_table_query).toMatchSnapshot();
  });

  test("creates table, inserts rows and selectes them", ({expect}) => {
    let query =
      [UsersTable.{id: "4", first: "Stephen", last: "King"}]
      |> UsersTableImpl.insert_many;

    let Ok(_) = pool |> Client.insert(~query) |> Lwt_main.run;

    let Ok(result) =
      pool
      |> Client.select_all(
           ~table=(module UsersTableImpl),
           ~query=select_all(),
         )
      |> Lwt_main.run;

    let user =
      result
      |> Array.map(row => {
           UsersTableImpl.(
             UsersTable.{
               id: row |> get_exn("id"),
               first: row |> get_exn("first"),
               last: row |> get_exn("last"),
             }
           )
         })
      |> Array.to_list
      |> List.find((user: UsersTable.t) => user.id == "4");

    expect.string(user.first).toEqual("Stephen");
    expect.string(user.last).toEqual("King");
  });
});
