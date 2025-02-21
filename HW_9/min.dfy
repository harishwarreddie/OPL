method find_min(a: array?<int>) returns (m: int)

  // Pre-condition: 
  // If the array is not null, it must have at least one element.
  // This allows us to safely access a[0] in the method body.

  requires a != null ==> a.Length > 0

  // Post-conditions:
  // 1. If the array is null, the method returns 0.

  ensures a == null ==> m == 0

  // 2. If the array is not null, the returned value is less than or equal to all elements in the array.
  //    This ensures that 'm' is indeed the minimum value.

  ensures a != null ==> forall i :: 0 <= i < a.Length ==> m <= a[i]

  // 3. If the array is not null, the returned value exists in the array.
  //    This ensures that we're returning an actual element from the array, not some arbitrary small value.

  ensures a != null ==> exists i :: 0 <= i < a.Length && m == a[i]

  // Note: The combination of conditions 2 and 3 effectively ensures that 'm' is the minimum value in the array.
{
  // Handle the null case first

  if a == null {
    return 0;  // As per the requirement, return 0 for null arrays
  }
  
  // Initialize 'm' with the first element of the array

  m := a[0];
  
  // Start the loop from the second element (index 1)

  var i := 1;

  // Loop through the array to find the minimum element

  while i < a.Length

    // Loop invariants:
    // 1. Ensure that 'i' stays within the bounds of the array

    invariant 1 <= i <= a.Length

    // 2. Ensure that 'm' is the minimum of all elements seen so far

    invariant forall k :: 0 <= k < i ==> m <= a[k]

    // 3. Ensure that the current minimum value 'm' exists in the portion of the array we've examined

    invariant exists k :: 0 <= k < i && m == a[k]
  {
    // If we find a smaller element, update 'm'

    if a[i] < m {
      m := a[i];
    }
    // Move to the next element

    i := i + 1;
  }

  // At this point, 'm' contains the minimum value of the entire array
  
}
