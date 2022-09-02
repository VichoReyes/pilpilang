# Type Intersections

It would be useful to write permissions like this:

```
can_write(u: User, r: Profile|Post|Repo) if r.user = u
```

Which gives `u` writing privilege for Profiles, Posts and Repos. To typecheck, it would need all those resources to have a .user field.

It could also be useful to define common groups, like

```
group Owned = Profile|Post|Repo

can_write(u: User, r: Owned) if r.user = u
```


