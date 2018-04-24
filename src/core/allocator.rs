#[cfg(target_arch="x86_64")]
use dynasmrt::x64::Assembler;
#[cfg(target_arch="x86")]
use dynasmrt::x86::Assembler;
use dynasmrt::DynasmApi;
use ::program::Integer;

const DYNAMIC_REGS: usize = 4;
#[cfg(target_arch="x86_64")]
const REG_ENCODINGS: &'static [u8; DYNAMIC_REGS] = &[8, 9, 10, 11];
#[cfg(target_arch="x86")]
const REG_ENCODINGS: &'static [u8; DYNAMIC_REGS] = &[3, 5, 6, 7];

#[derive(Debug, Clone)]
pub struct RegAllocator {
    allocations: [Option<RegAllocation>; DYNAMIC_REGS]
}

impl RegAllocator {
    pub fn new() -> RegAllocator {
        RegAllocator {
            allocations: [None; DYNAMIC_REGS]
        }
    }

    pub fn set_offset(&mut self, reg: u8, offset: i32) {
        let alloc = self.allocations.iter_mut().filter_map(|x| x.as_mut()).find(|x| x.reg == reg).unwrap();
        alloc.offset = offset;
        alloc.mutated = true;
    }

    pub fn modify(&mut self, reg: u8) {
        let alloc = self.allocations.iter_mut().filter_map(|x| x.as_mut()).find(|x| x.reg == reg).unwrap();
        alloc.mutated = true;
    }

    pub fn forget_offsets<F>(&mut self, filter: F) where F: Fn(i32) -> bool {
        for alloc in self.allocations.iter_mut() {
            if alloc.map_or(false, |x| filter(x.offset)) {
                *alloc = None;
            }
        }
    }

    pub fn forget(&mut self, reg: u8) {
        for alloc in self.allocations.iter_mut() {
            if alloc.map_or(false, |x| x.reg == reg) {
                *alloc = None;
            }
        }
    }

    pub fn spill_keep(&mut self, ops: &mut Assembler) {
        for alloc in self.allocations.iter_mut().filter_map(|x| x.as_mut()) {
            if alloc.mutated {
                dynasm!(ops
                    ; mov stack => Integer[alloc.offset], Ra(alloc.reg)
                );
                alloc.mutated = false;
            }
        }
    }

    pub fn spill_error(&mut self, ops: &mut Assembler) {
        for alloc in self.allocations.iter().filter_map(|x| x.as_ref()) {
            if alloc.mutated {
                dynasm!(ops
                    ; mov stack => Integer[alloc.offset], Ra(alloc.reg)
                );
            }
        }
    }

    pub fn spill_forget(&mut self, ops: &mut Assembler) {
        for alloc in self.allocations.iter_mut().filter_map(|x| x.take()) {
            if alloc.mutated {
                dynasm!(ops
                    ; mov stack => Integer[alloc.offset], Ra(alloc.reg)
                );
            }
        }
    }

    pub fn stage<'a>(&'a mut self, assembler: &'a mut Assembler) -> AllocationBuilder<'a> {
        AllocationBuilder {
            allocator: self,
            assembler: assembler,
            queue: Vec::new()
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct RegAllocation {
    offset: i32,
    reg: u8,
    mutated: bool
}

pub struct AllocationBuilder<'a> {
    allocator: &'a mut RegAllocator,
    assembler: &'a mut Assembler,
    queue: Vec<(&'a mut u8, bool, Option<i32>)>
}

impl<'a> AllocationBuilder<'a> {
    pub fn load(&'a mut self, reg: &'a mut u8, offset: i32) -> &'a mut Self {
        self.queue.push((reg, false, Some(offset)));
        self
    }

    pub fn free(&'a mut self, reg: &'a mut u8) -> &'a mut Self  {
        self.queue.push((reg, false, None));
        self
    }

    pub fn finish(&mut self) {
        let mut new_allocs = [None; DYNAMIC_REGS];
        let mut i = 0;
        // which allocations should be kept
        for &mut (ref mut reg, ref mut evaluated, offset) in self.queue.iter_mut() {
            if let Some(offset) = offset {
                if let Some(alloc) = self.allocator.allocations.iter_mut().find(|x| x.map_or(false, |x| x.offset == offset)) {
                    let alloc = alloc.take().unwrap();
                    // keep the allocation
                    new_allocs[i] = Some(alloc);
                    i += 1;
                    // return the allocated reg
                    **reg = alloc.reg;
                    // mark that this allocation is fulfilled
                    *evaluated = true;
                }
            }
        }

        // also keep topmost allocations
        for _ in 0 .. DYNAMIC_REGS - self.queue.len() {
            let mut state = None;
            for (i, alloc) in self.allocator.allocations.iter().enumerate() {
                if let &Some(alloc) = alloc {
                    if let Some((_, offset)) = state {
                        if offset < alloc.offset {
                            state = Some((i, alloc.offset));
                        }
                    } else {
                        state = Some((i, alloc.offset));
                    }
                }
            }

            if let Some((idx, _)) = state {
                let max = self.allocator.allocations[idx].take().unwrap();
                new_allocs[i] = Some(max);
                i += 1;
            } else {
                break
            }
        }

        // spill old mutated allocs
        for alloc in self.allocator.allocations.iter_mut() {
            if let Some(alloc) = alloc.take() {
                if alloc.mutated {
                    dynasm!(self.assembler
                        ; mov stack => Integer[alloc.offset], Ra(alloc.reg)
                    );
                }
            }
        }

        // load new allocs
        for (reg, _, offset) in self.queue.drain(..).filter(|x| !x.1) {
            let reg_enc = REG_ENCODINGS.iter().cloned().find(|r| new_allocs.iter().all(|x| x.map_or(true, |x| x.reg != *r))).unwrap();
            *reg = reg_enc;
            // load it?
            let offset = if let Some(offset) = offset {
                dynasm!(self.assembler
                    ; mov Ra(reg_enc), stack => Integer[offset]
                );
                offset
            } else {
                0
            };
            new_allocs[i] = Some(RegAllocation {
                reg: reg_enc,
                offset: offset,
                mutated: false,
            });
            i += 1;
        }

        self.allocator.allocations = new_allocs;
    }
}
