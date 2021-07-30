---
title: Separating Application Logic From Data
tags: programming, haskell
---

There are many annoying issues associated with any type of incremental software development associated with maintaining continuity. The API must be in sync with any type of frontend application, any RPC calls must be of a standardized format, and most importantly, the application must have an understanding of the database schema. It is this last hurdle that I aim to address today, not to wholly remedy - but propose efforts to minimize the issue.

---

At this point, some people will justifiably point to the growth of schema-less databases in the context of NoSQL and wonder if this issue is truly relevant to modern development. There are two points against this: the first, though the growth of NoSQL in industry has been astounding, it is still accounts for only a small minority of applications, second, although some NoSQL databases do not enforce a schema, the application architecture does. Even if we can change encoding on a whim for the database, it does not affect old data, and hence migration remains an issue. Indeed, the ability to change the future data format so freely can actually lead to a less consistent database as there is no immediate necessity to perform a proper migration.

---

## Automatic Migrations

One of the most standard solutions to this issue is to run automatic migrations on each iteration of the application to modify the database appropriately. This is a common feature in most database reliant frameworks, and has the advantage of not requiring developer input to make a modification to the database. A developer can change their data storage and feel reasonably confident that their software will make the necessary changes. There are several complications associated with this however, some of which are enumerated below.

### Automatic Database Schmeas

Now, in many cases this is not an issue at all. For many projects, it's not necessary to have fine tuned control over the database, and an inexperienced developer is certainly more likely to make a hash of it than their application framework on their first few tries. However, in the case when it *is* necessary, as often happens later in a project the inability to directly define the database can be a complication. Moreover, even in the best case scenario, one is limited by what the framework used for migrations is capable of. Invariably, this is much less than the database can do, and so accepting the generated schema immediately restricts the power of the database used. Consider the following example.

Suppose that when you create a user in the database you want to send an email to a user that allows them to log in using a specific key to set a password. This process should be automatic with every user, and we can specify the logic entirely within the database.

```sql
CREATE TABLE users(
    -- We can specify the use of uuid's via "uuid-ossp"
    id UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),

    -- Note that the username is case insensitive, via "citext"
    username CITEXT NOT NULL

    -- We intentionally map to a @Maybe@ type to represent new users.
    password TEXT
);

CREATE TABLE password_reset_tokens(
    token UUID PRIMARY KEY NOT NULL DEFAULT gen_random_uuid(),
    user_id    UUID REFERENCES USERS ON DELETE CASCADE
);

-- Generate a new token after a user has been inserted
CREATE OR REPLACE FUNCTION new_user_token() RETURNS TRIGGER AS $$
    BEGIN
        INSERT INTO password_reset_tokens(user_id) VALUES (NEW.id);
        RETURN NULL;
    END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER new_user AFTER INSERT ON users
FOR EACH ROW EXECTURE PROCEDURE new_user_token();

-- When we reset a user's password, we should remove their password reset token
-- automatically .
CREATE OR REPLACE FUNCTION remove_expired_tokens() RETURN TRIGGER AS $$
    BEGIN
        IF (NEW.password = OLD.password) THEN
           DELETE FROM password_reset_tokens WHERE user_id = NEW.id ;
        END IF;
        RETURN NULL;
    ENd;
$$ LANGUAGE plpgsql;
CREATE TRIGGER check_pass_reset AFTER UPDATE ON users
FOR EACH ROW EXECTURE PROCEDURE remove_expired_tokens();
```

Let's look at what the above accomplishes. Not only do we get a free token every time we create a user, but if a user updates their password any and all of their old password reset tokens are invalidated. Now, writing this in the database is quite a bit more cumbersome, but it has tremendous advantages in a system's stability and security. In this case the very structure of the data model prevents the occurrence of an accidentally immortal password reset t token (there are other loopholes, but this is only an example after all).

Of course, doing this in practice requires some attention to the specific database functionality and is most certainly not suited for all applications or projects. But, in those cases where it is practical it is an enormous benefit - and the inability to mix such behavior, which is far more complicated than a standard schema, with automated migrations is a severe drawback.

### Unintentional Mistakes

One of the great features of automated data migrations is the fact that they just work. Out of the box, you only need to work with your own code, and have some faith that whatever software is responsible for migrating the schema is reasonable. There is a downside to this however. Because every application deployment has the potential to result in a database alteration, there is a real risk to the application stability. This can be countered with extensive review and testing, but it means that each iteration of the application must pass the requirements of modifying the entire database - which can massively impede development.

The last point is key, because any attempt to make the application responsible for **accessing** the database also responsible for **defining** it's schema leads to this complication. It bundles the application logic with the database maintenance and ensures that maintenance of data is bundled within application maintenance. Fortunately, there is a better way.

## Separating Concerns

The difficulties arrived at in maintaining the database from within an application naturally suggests a solution: separating the database schema from the application logic. In practice this is often accomplished by the creation of an internal library which provides types and accessors for the database but does not expose any of the internal query logic.

The specification of a **library** as our database access point is more than just a convenient delineation in most module systems, but also provides a convention for updates to the database. For instance, after a complete overhaul of the database, the previous library's can be deprecated, and application developers will get deprecation errors. A small feature, which is backwards compatible, can advance the library and only needs incremental adoption.

However, most importantly the separation allows the application developer to work with what they have from the data library and iterate without fear of causing a major failure in the database. The biggest risks of automatic migrations are avoided. Of course, there are drawbacks. Building such a dedicated database library means migrations are most likely to be manual. However, this is not as much of a hardship as it would be in a bundled application, since the actual occasions where the database needs to be modified are rather rare.

### Type Safety

There is another opportunity presented in the separation of concerns: that of type safety. In many applications, we tend to define a type that is intentionally obscure, **except** to the database:

```haskell
module User.Types
    ( Username
    , mkUsername
    , pattern Username )
where

-- Our type we wish, to hide the constructor and only expose a function
-- for valid usernames
newtype Username = UnsafeUsername Text
    deriving (Generic, ToSQL, FromSQL) -- Except, we now have thrown
                                       -- away all security, since any
                                       -- Joe can construct our type
                                       -- using the SQL typeclasses.

-- Sometihng to pattern match against (this is a useful feature
-- allowing us to pattenr match on the constructor without
-- actually revealing it).
pattern Username <- UnsafeUsername

-- The only constructor we'd like to expose, something that makes some
-- guarantees about our username. Sadly, we could bypass this using
-- FromSQL.
mkUsername :: Text -> Maybe Username
mkUsername = ...
```

Although there are gains to this type safety, there is still a loophole, we can construct a Username from a raw string using the `FromSQL` class - but we can not leave this class since we must have a way of retrieving our types from the database.

We can, however, circumvent this in the case of our database library, by, quite simply, not exposing the `FromSQL` class! Since all types that are stored are unique to the database library, the application developer has no ability to circumvent the type safe constructors. We are guaranteed that all `Username` types are rightly created with `mkUsername`, and furthermore we haven guaranteed that all such `Username` types used in the data library are valid.

# TL;DR

The major drawback of automatic migrations is the entanglement of database schema with application logic. It prevents the full usage of the database while also forcing the application to take responsibility for the entirety of the projects data. The latter both impedes development speed, and when not, is severely dangerous. By separating the database accessors into a separate library, we eliminate the responsibility for the database as a concern for application development, thus allowing much faster and safer development. Moreover, we separate the implementation of our data into a different project, hence allowing more full control of the database if so desired. While this can mean more manual migrations, these are far less common since the database accessors are less likely to be modified frequently, and provide much greater stability and control.

---

> Thanks for reading the above rant. It is both longer and more rambling than first intended. I blame the coffee.

```
      )  (
     (   ) )
      ) ( (
    _______)_
 .-'---------|
( C|/\/\/\/\/|
 '-./\/\/\/\/|
   '_________'
    '-------'
```
