const std = @import("std");
const panic = std.debug.panic;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

// @Info: let's go for a bucket size of 64 elements. Obviously this is not the best, but it is a good start

// @Info: this bucket array delegates allocations to the upper structure that is owning the bucket
pub fn BucketArray(comptime T: type, comptime cap: usize) type
{
    return struct
    {
        items: []T,
        len: usize,

        const Self = @This();
        const BucketArrayAllocError = error
        {
            BucketArrayIsFull,
        };
        fn append(self: *Self, new_item: T) Self.BucketArrayAllocError! *T
        {
            if (self.len < cap)
            {
                const index = self.len;
                self.items[index] = new_item;
                self.len += 1;
                return &self.items[index];
            }

            return BucketArrayAllocError.BucketArrayIsFull;
        }

        fn init(allocator: *Allocator) !Self
        {
            const bucket_buffer = try allocator.alloc(T, cap);

            const bucket_array = Self {
                .items = bucket_buffer,
                .len = 0,
            };
            return bucket_array;
        }
    };
}

pub fn BucketArrayList(comptime T: type, comptime cap: usize) type
{
    return struct
    {
        list: ArrayList(BucketArray(T, cap)),
        current_index: usize,
        allocator: *Allocator,

        const Self = @This();

        pub fn append(self: *Self, new_element: T) !*T
        {
            return self.list.items[self.current_index].append(new_element) catch |err|
            {
                const new_bucket = try BucketArray(T, cap).init(self.allocator);
                try self.list.append(new_bucket);
                self.current_index += 1;
                // @Info: this shouldn't ever fail because the memory is already allocated (it's static memory)
                return self.list.items[self.current_index].append(new_element) catch unreachable;
            };
        }

        pub fn len(self: *Self) u64
        {
            return (self.current_index * 64) + self.list.items[self.current_index].len;
        }

        pub fn get(self: *Self, index: u64) ?*T
        {
            const bucket_index = index / cap;
            if (bucket_index <= self.current_index)
            {
                const bucket = &self.list.items[bucket_index];
                const element_index = index % cap;
                if (element_index < bucket.len)
                {
                    const result = &bucket.items[element_index];
                    return result;
                }
            }

            return null;
        }

        pub fn init(allocator: *Allocator) !Self
        {
            var bucket_arraylist = Self {
                // @Info: this bucket array is supposed to hold a lot of elements, that's why we are allocating 32 * cap elements
                .list = try ArrayList(BucketArray(T, cap)).initCapacity(allocator, 32),
                .current_index = 0,
                .allocator = allocator,
            };


            const new_bucket = try BucketArray(T, cap).init(allocator);
            try bucket_arraylist.list.append(new_bucket);

            return bucket_arraylist;
        }
    };
}
