# x86_64 context switching for Jet scheduler (Linux/Windows symbol form)
# Arguments:
#   %rdi - pointer to current Context (to save)
#   %rsi - pointer to next Context (to restore)

.text
.globl jet_context_switch
jet_context_switch:
    # Save current context (first argument - %rdi)
    movq %rsp, 0(%rdi)      # Save stack pointer
    movq %rbp, 8(%rdi)      # Save base pointer
    movq %rbx, 16(%rdi)     # Save rbx
    movq %r12, 24(%rdi)     # Save r12
    movq %r13, 32(%rdi)     # Save r13
    movq %r14, 40(%rdi)     # Save r14
    movq %r15, 48(%rdi)     # Save r15
    movq (%rsp), %rax       # Get return address from stack
    movq %rax, 56(%rdi)     # Save instruction pointer

    # Restore next context (second argument - %rsi)
    movq 0(%rsi), %rsp      # Restore stack pointer
    movq 8(%rsi), %rbp      # Restore base pointer
    movq 16(%rsi), %rbx     # Restore rbx
    movq 24(%rsi), %r12     # Restore r12
    movq 32(%rsi), %r13     # Restore r13
    movq 40(%rsi), %r14     # Restore r14
    movq 48(%rsi), %r15     # Restore r15
    movq 56(%rsi), %rax     # Get new instruction pointer
    movq %rax, (%rsp)       # Place it as return address

    ret

# Initialize a new context for a task
# Arguments:
#   %rdi - pointer to Context to initialize
#   %rsi - stack pointer (top of stack)
#   %rdx - entry point function
.globl jet_context_init
jet_context_init:
    movq %rsi, 0(%rdi)      # Set stack pointer
    movq %rsi, 8(%rdi)      # Set base pointer (same as stack)
    xorq %rax, %rax
    movq %rax, 16(%rdi)     # Clear rbx
    movq %rax, 24(%rdi)     # Clear r12
    movq %rax, 32(%rdi)     # Clear r13
    movq %rax, 40(%rdi)     # Clear r14
    movq %rax, 48(%rdi)     # Clear r15
    movq %rdx, 56(%rdi)     # Set instruction pointer to entry point
    ret

# Get the current stack pointer
.globl jet_get_stack_pointer
jet_get_stack_pointer:
    movq %rsp, %rax
    ret
